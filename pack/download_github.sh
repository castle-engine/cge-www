# -----------------------------------------------------------------
# Utilities to make releases on non-GitHub platforms (like itch.io)
# from GitHub releases.
#
# Use by "source download_github.sh".
# Works within bash strict mode (set -eu).
# -----------------------------------------------------------------

# Download a release from GitHub.
#
# $1 - GitHub organization/repository name, like "castle-engine/castle-engine".
# $2 - GitHub release/tag name, like "v1.2.3" or "snapshot".
# $3 - File name to download, like "castle-engine-1.2.3-win64.zip".
#
# Downloaded file is placed in the current directory.
#
# Use-case: For other channels (itch.io, sourceforge)
# we download the release from GitHub and upload it somewhere else.
# ( We don't run ./pack_release.sh script from upload_itch_io.sh or such,
# to avoid possibility of building with different environment,
# e.g. building on different Linux means linking to different GLIBC. )
download_github_release ()
{
  GIT_ORG_REPO="$1"
  GIT_TAG="$2"
  FILE_NAME="$3"
  shift 3

  echo '-------------------------------------------------------------'
  echo "Downloading $FILE_NAME from GitHub $GIT_ORG_REPO release $GIT_TAG"

  # Remove file if it already exists.
  rm -f "$FILE_NAME"

  # wget is one option.
  # wget "https://github.com/${GIT_ORG_REPO}/releases/download/"${GIT_TAG}"/${FILE_NAME}"

  # GH CLI is another option.
  # Requires installing https://cli.github.com/ and authenticating.
  # May be more reliable? Or faster? Testing.
  gh auth status
  # gh release list --repo "$GIT_ORG_REPO"
  # gh release view --repo "$GIT_ORG_REPO" "$GIT_TAG"
  gh release download --repo "$GIT_ORG_REPO" "$GIT_TAG" --pattern "$FILE_NAME"

  echo 'MD5 checksum:'
  md5sum "$FILE_NAME"
}

# Unpack the archive (zip,tar.gz) and remove the archive too.
# Works on archive in current directory, and unpacks it in current directory.
#
# $1 - archive file name (without path).
#
# Useful to upload to itch.io:
#
# - Becase butler prefers unpacked files.
#   Otherwise, it will treat zip differently (will unpack it before uploading)
#   than tar.gz (will treat it as single file, internally: directory with 1 file).
#   See https://itch.io/docs/butler/single-files.html
#
# - We also unpack, to place .itch.toml file inside, during butler_push.
unpack_archive ()
{
  local FILE_NAME="$1"
  shift 1

  local FILE_EXTENSION=".${FILE_NAME##*.}"

  # Note: we unpack with less verbose options, to not flood script
  # with lots of output (our CGE contains many files)
  # and thus obscure MD5 checksums (which are useful to see and compare).

  case "$FILE_EXTENSION" in
    .zip)
      unzip -q "${FILE_NAME}"
      # verbose:
      # unzip "${FILE_NAME}"
      ;;
    .gz) # assuming .tar.gz
      tar xzf "${FILE_NAME}"
      # verbose:
      # tar xzvf "${FILE_NAME}"
      ;;
    *)
      echo "Cannot deal with file $FILE_NAME, extension $FILE_EXTENSION"
      exit 1
      ;;
  esac

  rm -f "${FILE_NAME}"
}

# Upload to itch.io release for one platform, after downloading it from GitHub.
#
# $1 - itch.io application org/name, like 'castle-engine/castle-game-engine'.
# $2 - itch.io manifest file name.
#      Must be an absolute path (add e.g. `pwd` if needed).
# $2, $3, $4 - arguments for download_github_release, see docs of it.
#
# $@ - additional arguments for "butler push ...".
do_upload_itch_io ()
{
  local ITCH_IO_NAME="$1"
  local MANIFEST="$2"
  # arguments for download_github_release
  local DOWNLOAD_GIT_ORG_REPO="$3"
  local DOWNLOAD_GIT_TAG="$4"
  local DOWNLOAD_FILE_NAME="$5"
  shift 5

  UPLOAD_DIR=/tmp/upload_itch_io_unpacked_$$
  rm -Rf "${UPLOAD_DIR}"
  mkdir "${UPLOAD_DIR}"
  cd "${UPLOAD_DIR}"

  download_github_release "${DOWNLOAD_GIT_ORG_REPO}" "${DOWNLOAD_GIT_TAG}" "${DOWNLOAD_FILE_NAME}"

  echo '-------------------------------------------------------------'
  echo 'Unpacking'

  unpack_archive ${DOWNLOAD_FILE_NAME}"
  cp -f "${MANIFEST}" "${UPLOAD_DIR}"/.itch.toml

  echo '-------------------------------------------------------------'
  echo 'Uploading'

  butler push "${UPLOAD_DIR}" "$@"

  # clean here, to not consume disk space once script exits
  rm -Rf "${UPLOAD_DIR}"
}
