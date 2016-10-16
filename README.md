# CalVAD map ui, Elm version

This is the CalVAD map user-interface, using
mostly [Elm](http://elm-lang.org/) apps and D3.

# seems to work

As of 2016-10-06, I have this up and running mostly, but for tweaking
and bug hunting

# running while developing

If developing, one item you have to do manually is link the
cellmembership json file so that the UI can get it from the static
file server:

After npm install, you should have node_modules/calvad_areas

then do

```
cp  node_modules/calvad_areas/lib/cellmembership.json src/static/grid/.
```

This will put cellmembership.json file into place for testing the UI
using `npm start`


So, with that done, you can set up the live file reloader by running
`npm start`, and then generate the real UI by doing `npm run build`.
The final built UI should be copied into the running website by
something like:

```
rsync -av dist/. myserver.com:repos/calvad/grid_display/public/.
```

Where you replace "myserver.com" with the address of the server for
the website.  If its on the same machine as developing, you can
probably just do:

```
rsync -av dist/. /path/to/code/for/grid_display/public/.
```
