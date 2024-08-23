# -----------------------------------------------------------------
# Utilities to download a release from GitHub.
#
# Use-case: For other channels (itch.io, sourceforge)
# we download the release from GitHub and upload it somewhere else.
# ( We don't run ./pack_release.sh script from upload_itch_io.sh or such,
# to avoid possibility of building with different environment,
# e.g. building on different Linux means linking to different GLIBC. )
#
# Use by "source download_github.sh".
# Works within bash strict mode (set -eu).
# -----------------------------------------------------------------

# Download a release from GitHub.
# $1 - GitHub organization/repository name, like "castle-engine/castle-engine".
# $2 - GitHub release/tag name, like "v1.2.3" or "snapshot".
# $3 - File name to download, like "castle-engine-1.2.3-win64.zip".
# Downloaded file is placed in rthe current directory.
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
