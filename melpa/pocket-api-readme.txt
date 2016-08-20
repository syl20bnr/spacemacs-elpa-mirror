The usage is similar with [[https://github.com/pterygota/el-pocket][el-pocket]].

The first time using `pocket-api', you should execute =pocket-api-authorize= twice.

1. The first time execute =pocket-api-authorize= you will be directed to the oauth/request page, where you can click on authorize. After authorizing, you may see an error page, but it don't matter.

2. And then, the second time execute =pocket-api-authorize= you will get the access token, and it will be saved to =~/.el-pocket-auth.json=

After that, you don't need to do the authorizing job any more, just use =(el-pocket-load-auht)= to reload the access token.

Usng =M-x el-pocket-add= to add URLs
