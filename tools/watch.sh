#!/bin/sh

cd "$(dirname "$0")/.."

inotifywait -m -r -q -e modify,create,move \
    --include '^./(src|libc|freestanding|meta|tests).*\.(c|h|peg|enc|py)$' . |
  while read -r dir action file; do
		clear
		echo "$action" "$dir$file"

		# Sometimes the editor gives multiple events in quick succession when the
		# file is saved -- maybe it's saving and then running the formatter and
		# then saving again? This will eat all output from inotifywait for the next
		# second to "debounce" it.
		timeout 1 cat > /dev/null

	  cmd="./build.py "$@""
		echo Running command "$cmd"
		$cmd
		echo Done
	done
