I'm not great at regexes so this code is designed around that limitation.
The resulting plist may seem oddly structured but ivy-read uses the first element of each element as the displayed title.
So the strucuture is '(title-for-ivy :title title :url url).  A quick cdr returns a "proper" plist.
