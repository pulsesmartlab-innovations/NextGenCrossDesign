# Hosting NextGenCrossDesign for multiple users (ShinyProxy)

This directory documents how to deploy the workbench as a multi-user web app
with **ShinyProxy**, which gives every user their own Docker container — separate
filesystem, separate R process. That per-user isolation is the model the app is
built for, so cross-user collisions are impossible at the infrastructure layer
and the app code stays simple.

The same package still installs and runs unchanged for an individual on a laptop
(`run_workbench()`); nothing here is required for local use.

## Files

| File | Purpose |
| --- | --- |
| `Dockerfile` | Builds one image containing **both** the backend engine and this front-end. |
| `config.yml` | Workbench config baked into the image (overridable by `NGCD_*` env vars). |
| `application.yml` | ShinyProxy server config: the app spec + Docker backend. |
| `.dockerignore` | Keeps the build context small. |

## Why one image holds both packages

The workbench runs the backend **out-of-process**: it writes a run config + input
CSVs into a run directory, invokes the backend's headless `Rscript` runner via
`system2(..., timeout = ...)`, and reads back `result.json` + figures. That gives
process isolation, a hard timeout, cancel, and reproducible artifacts — without a
network service. It does mean the backend must be a real, installed R package
reachable via `Rscript` in the same container. So the image installs:

- `nextgenCrossDesign` — the backend engine (compiles a C++ kernel; needs a
  toolchain, which the Dockerfile installs).
- `nextgenCrossWorkbench` — this Shiny front-end.

## Published image (skip the build)

A pre-built, **multi-arch** image (`linux/amd64` + `linux/arm64`) is published to
GitHub Container Registry:

```
ghcr.io/pulsesmartlab-innovations/ngcd-workbench:0.15.2   # (also :latest)
```

`application.yml` already points at this path, so on most hosts you do **not**
need to build anything — the daemon pulls the arch matching the host. What the
ShinyProxy host needs to pull depends on the package's visibility:

- **If the package is public** — no auth needed. The host's Docker daemon pulls
  it directly; nothing else to configure.

- **If the package is private** (the default for a freshly pushed package) — the
  host's Docker daemon must authenticate once with a token that has
  `read:packages`:

  ```bash
  echo <PAT-with-read:packages> | docker login ghcr.io -u <user> --password-stdin
  ```

To change visibility: **Packages → ngcd-workbench → Package settings → Change
visibility**. The package's visibility is independent of the source repositories'
visibility — making the package public does not expose the repos. (`ngcd-workbench`
is owned by the `pulsesmartlab-innovations` **user** account, so it lives under
that account's Packages, not an organization's.)

Rebuild from source (below) only when you change the front-end or backend.

## Build from source

The backend repo is private, so the image installs both packages from **source
tarballs** copied into the build context (no GitHub token ends up in the image).

```bash
# 1. Build the backend tarball (from a checkout of the backend repo)
#    -> produces nextgenCrossDesign_0.9.0.tar.gz
R CMD build /path/to/nextgenCrossDesignR

# 2. Build the front-end tarball (from a checkout of THIS repo)
#    -> produces nextgenCrossWorkbench_0.15.2.tar.gz
R CMD build .

# 3. Put both tarballs at the repo root (the build context) and build the image
docker build -t ngcd-workbench:0.15.2 \
  --build-arg BACKEND_TARBALL=nextgenCrossDesign_0.9.0.tar.gz \
  --build-arg FRONTEND_TARBALL=nextgenCrossWorkbench_0.15.2.tar.gz \
  -f deploy/Dockerfile .
```

Pin the image tag to the front-end version and keep `required_backend_version`
in `config.yml` in step with the backend tarball you install.

### Publish a new multi-arch image to ghcr

The single-arch `docker build` above is fine for local testing. To publish the
`amd64` + `arm64` image consumed by `application.yml`, use `buildx` with both
tarballs already at the context root:

```bash
# One-time on the build host: a container-driver builder + QEMU for the arch
# your host does NOT run natively.
docker run --privileged --rm tonistiigi/binfmt --install all
docker buildx create --name ngcd-builder --driver docker-container --use

echo <PAT-with-write:packages> | docker login ghcr.io -u <user> --password-stdin
docker buildx build --builder ngcd-builder \
  --platform linux/amd64,linux/arm64 \
  -t ghcr.io/pulsesmartlab-innovations/ngcd-workbench:0.15.2 \
  -t ghcr.io/pulsesmartlab-innovations/ngcd-workbench:latest \
  --push \
  --build-arg BACKEND_TARBALL=nextgenCrossDesign_0.9.0.tar.gz \
  --build-arg FRONTEND_TARBALL=nextgenCrossWorkbench_0.15.2.tar.gz \
  -f deploy/Dockerfile .
```

The leg for the non-native arch runs under emulation and compiles the full
R/Shiny/C++ stack from source, so it is slow — expect a long build.

## Run under ShinyProxy

1. Install ShinyProxy on the host (a JAR + a Docker daemon), per
   <https://shinyproxy.io/documentation/>.
2. Merge the `proxy` block from `application.yml` into the host's
   `application.yml`. It declares one app spec (`id: ngcd`) pointing at the
   `ngcd-workbench` image on container port **3838** (what the app serves on).
3. Start ShinyProxy. Each user who opens the app gets a fresh container; when
   their session ends, the container — and everything in it — is destroyed.

ShinyProxy starts **one container per user session by default**; you do not
configure isolation, you get it.

## How artifacts behave (within-session)

Run files (`config.json`, inputs, `result.json`, figures, the HTML/PDF report)
live under the container's `data_dir`, which defaults to a per-session temp
directory. When the user's session/container ends, they are gone. This is
intentional for a hosted deployment: no cross-user paths, nothing to garbage
collect. The app tells users to download anything they want to keep, and the
download buttons stream files straight to the user's browser (not via shared
server storage).

If you genuinely need runs to survive across sessions, mount per-user persistent
storage and point `NGCD_DATA_DIR` at it (via `container-env` in the spec, or a
ShinyProxy volume mount). Then set `keep_runs` to a sensible cap so the volume
doesn't grow forever.

## Configuration knobs

Every `config.yml` key is overridable at runtime by `NGCD_<UPPERCASE_KEY>`, so
you can tune a deployment from `application.yml`'s `container-env` without
rebuilding:

| Key / env | Default | Notes |
| --- | --- | --- |
| `deployment_mode` / `NGCD_DEPLOYMENT_MODE` | `server` (in this image) | `server` = ephemeral per-session storage + the within-session download note. `local` (the package default off-server) keeps a persistent `ngcd-data` folder. |
| `data_dir` / `NGCD_DATA_DIR` | per-session temp dir (server mode) | Writable base for runs, reports, presets. Blank = the mode default. |
| `keep_runs` / `NGCD_KEEP_RUNS` | `20` | Max run dirs kept per session (`0` = unlimited). |
| `rscript_path` / `NGCD_RSCRIPT_PATH` | `Rscript` | Backend interpreter; on PATH inside the container. |
| `required_backend_version` | `0.7.0` | Floor for progressive enhancement; the image bundles backend 0.9.0. |
| `developer_mode` / `NGCD_DEVELOPER_MODE` | `false` | Keep false on a hosted server (hides Setup/plumbing). |

## Updating the backend

Rebuild the image with a newer backend tarball (and bump
`required_backend_version` in `config.yml`), then re-deploy. The front-end picks
up new/changed backend parameters automatically — it calls the backend by name
and filters arguments against the installed backend's live formals — so most
backend updates need no front-end change. See `../tools/update-backend.R` for the
non-container update path.

## Scaling notes

- One ShinyProxy host serves a lab or department comfortably; each user is one
  container. Set `container-memory-limit` per spec to bound resource use.
- For elastic "worldwide" load (many concurrent users, autoscaling across
  machines), ShinyProxy's Kubernetes backend (`proxy.container-backend:
  kubernetes`) or a REST/queue service in front of the backend is the next step
  — a later evolution, not needed for a single-host multi-user deployment.
- Put ShinyProxy behind a reverse proxy (nginx/Traefik) for TLS, and enable a
  real `authentication` backend + `access-groups` before exposing it publicly.
