# This file is not meant to be run directly. Instead,
# it should be sourced by the root `install.sh` script.

echo "Installing i3blocks config..."

link_path "$(pwd)/config" "${HOME}/.config/i3blocks"
