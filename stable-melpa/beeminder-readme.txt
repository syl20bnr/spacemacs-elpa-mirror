beeminder.el provides a simple way for Emacs to interact with the Beeminder
API. It's pretty basic at the moment, but can be used to fetch and submit
data.

Please set `beeminder-username' and `beeminder-auth-token' before using.

You can find your auth token by logging in to Beeminder and then visiting the
following URI: https://www.beeminder.com/api/v1/auth_token.json

Keyboard bindings:

All keyboard bindings have a C-c b prefix

C-c b g    - Insert your goals as an org-mode list
C-c b m    - Display username in message line
