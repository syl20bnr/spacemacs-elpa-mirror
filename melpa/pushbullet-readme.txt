Pushbullet is an android app that handles notifications. Luckily
there's an API which we can use to push stuff from your favourite
editor to your phone

At the moment this uses `grapnel' library for http requests. This
is just an experiment, any comments and suggestions are more than
welcome. Customize the variable `pushbullet-api-key' in the group
`pushbullet' to match your api-key. At present calling
`pushbullet' interactively with a selection will send that
selection with the user specified title to your android app
if region is inactive it will send the whole contents of buffer
to the app
