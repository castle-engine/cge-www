Right now **all access to Castle Game Engine API must happen from the same thread**.

Many other libraries, including Lazarus LCL, or Unity, have the same limitation, so we consider this limitation acceptable for now.

Important use-case:

- The usual use-case for multi-threading in games is to have perfectly smooth rendering (of loading screen, or even of the interactive game) while at the same time loading some resources to GPU. This isn't possible now using Castle Game Engine. Things like "loading screen" must be done in discrete steps now (see e.g. `StateLoading` in [examples/user_interface/zombie_fighter](https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/zombie_fighter)), and it is unavoidable that the process will just have to wait for some steps to finish, so your loading screen will not have 60 FPS.

Future:

- Asynchronous loading will be possible some day. 

- This requires renderer capable of multi-threading, like Vulkan.

- We prefer exposing asynchronous calls instead of just "allowing to use CGE from multiple threads". So you will still be required to call all CGE from main thread, however we will expose methods like `TCastleScene.PrepareResourcesAsynchronously` that call some callback in main thread when the loading finished. This is a more limited approach, but also easier to implement (greater flexibility for the implementation, because we know that only specific things are done in the background -- not that potentially everything is in different thread). It is also in practice better for some users -- using threads is *hard* to do 100% correctly (without making any mistakes in data access, that result in very-rare-but-possible-and-painful-to-debug crashes).

    Also `TCastleScene.LoadAsync(..., OnFinish: TNotifyEvent)`. As with `TCastleDownload`, it would internally use threads or not -- this fact would be hidden from users.  This approach gives simple API, allows engine to do it internally in various ways, and doesn't force users to deal with dangers of multi-threading (similar to Unity async loading).

Notes about parallelism already possible/done in CGE:

- Some parallelism is already happening, by rendering done in parallel (which is hidden from us by OpenGL), audio playing in parallel (which is hidden from us by OpenAL), some physics processing done in parallel (which is hidden from us by Kraft), and downloading in the background (which is hidden from us by `TCastleDownload`). This approach, while more limited, allows to keep the engine simple and efficient, without worrying in every API call "what if some other thread is doing something with my data in parallel".

- You can also of course use threads (e.g. through Pascal's `TThread`, `threadvar` and other constructs) to do your own work, e.g. calculating something or loading files from disk to a stream. FPC standard library is thread-safe, in the sense that you can use e.g. one `TFileStream` in one thread, another `TFileStream` in another thread, without any need to synchronize (but remember they must be different `TFileStream` instances). Just make sure to synchronize (i.e. call from the main thread) the place where you pass these `TFileStream` contents to CGE. 

Future challenges:

- All OpenGL(ES) commands must be done from the same thread anyway. This is a limitation of OpenGL(ES).

    So to e.g. load textures *to GPU* while doing a smooth rendering from another thread, we would need to implement another renderer some day. E.g. Vulkan allows to use multi-threading ( https://www.khronos.org/registry/vulkan/specs/1.0/html/vkspec.html#fundamentals-threadingbehavior , https://developer.nvidia.com/sites/default/files/akamai/gameworks/blog/munich/mschott_vulkan_multi_threading.pdf , https://www.reddit.com/r/vulkan/comments/52aodq/multithreading_in_vulkan_where_should_i_start/ ). Although the burden of protecting multi-thread access in Vulkan lies in the hands of user, so it's not something trivial to take advantage of anyway, but at least it becomes possible.

- Right now, some engine code uses global caches (for texture/audio data, for X3D prototypes, and much more). If we decide to make some of them thread-safe, they will have to be reworked (so, more complicated code and possibly a little slower) or be separate per thread (so, less caching -> less gains).

- The CGE will probably never be thread-safe in the sense that "you can access *anything* from multiple threads simultaneously". This would require a lot of safeguards and code complications, more than we can realistically maintain. Again, software larger than CGE (like LCL or Unity) also didn't dare to do this. 

    Maybe we can make (but even this is far future) some clear separation, e.g. being able to use classes like `TCastleScene`, `TDrawableImage` from separate threads, but only if you use different instances of them. 

    Although I'm leaning more toward only having specific operations asynchronous, like `PrepareResourcesAsynchronously`, thus "hiding" threads in simple (but more limited, but also easier to implement) API. Thus, it is possible that access to CGE will remain limited to "only a single thread" forever, but we will expose APIs to do asynchronous loading of specific stuff like images or scenes.
