 #!/bin/bash

import -window "$(xdotool getwindowfocus -f)" $HOME/$(date +%F-%H%M%S_%N).png
