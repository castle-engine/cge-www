# Exporting from Blender to X3D

A collection of hints about using the Blender X3D exporter. They apply to the standard X3D exporter distributed with [Blender](http://blender.org/) and to our modified exporter version available from https://castle-engine.io/creating_data_blender.php .

**Obsolete: Many notes below are relevant only to the exporter state in Blender 2.7. With Blender 2.8, the exporter is unfortunately even more limited (for now), e.g. it doesn't support textures assigned in Blender 2.8, even the base texture. With Blender 2.8, we advise exporting to glTF instead. See https://castle-engine.io/creating_data_blender.php for Blender + Castle Game Engine instructions.**

Table of Contents
=================

* [General notes](#general-notes)
* [Materials](#materials)
* [Textures](#textures)
* [Extra material textures (normal maps, specular maps) with CommonSurfaceShader](#extra-material-textures-normal-maps-specular-maps-with-commonsurfaceshader)


## General notes

* It **does not support animations**. To export animations from Blender that can work in [Castle Game Engine](https://castle-engine.io/) and [view3dscene](https://castle-engine.io/view3dscene.php), use our [Blender castle-anim-frames exporter](https://castle-engine.io/creating_data_blender.php) instead. The Castle-anim-frames exporter internally exports to a series of X3D files, so the rest of this page still matters to you anyway:)

* Latest exporter **honours the <tt>Backface culling</tt> setting in Blender**. It makes the back faces invisible. If it's not OK, try flipping the normals (*"Recalculate Normals"*, Ctrl + N, Ctrl + Shift + N), or turning the "backface culling" off. You can find the appropriate checkbox *"Backface Culling"* at the material settings once you change the Blender UI mode (at the header, default is *"Blender Render"*) to *"Blender Game"*.

* **Smoothing works**. You can click *"Smooth"* or *"Flat"* on the whole mesh, you can use the "Auto Smooth" checkbox and *"Angle"* as well. **But do not use the <tt>Triangulate</tt> option at export** (it messes the smoothing, making the mesh always completely smoothed; yes, it's a bug in X3D exporter, it should not use the <tt>IndexedTriangleSet</tt> or it should split the vertexes). Note that if you don't export the explicit "Normals", you should make the ordering of your faces consistent (regardless if you use "backface culling" or not) by occasionally using the "Recalculate Normals" (Ctrl + N) feature.

* **"Vertex painting" is not supported**.

* **Points and spot lights are exported OK. Directional light ("sun") is incorrect in official exporter version.** Use our exporter version from https://castle-engine.io/creating_data_blender.php to have <tt>DirectionalLight</tt> working OK.

* **Camera is exported, but it's gravity up vector will be messed up**. It is exported is such way that the gravity up vector (which is also used as the main rotation axis in some camera modes) will be set to whatever is the camera's current up vector (instead of being controlled independently from camera up). In view3dscene, use the *"Navigation -> Set Up (and Gravity Up) to +Y"*.

* **Modifiers are exported OK**. So you don't need to *"Apply"* modifiers like *Mirror* or *Subdivision Surface* before exporting.

* **Other small improvements of our exporter version from https://castle-engine.io/creating_data_blender.php**:
  * Exports *Clamp X / Y* texture settings. Useful when you don't want your texture to "repeat".
  * Exports "Shadeless" material (only emit color) in a way that activates an optimized "shadeless" rendering path in *Castle Game Engine*.

## Materials

* The exporter **supports  multiple materials (and so, also textures) on the same mesh**. Internally, such Blender objects will be split into multiple X3D Shapes (since X3D does not allow to use multiple materials on the same mesh, for speed). But this detail is usually not important for you.

* **Assign some material to your meshes, or they will be unlit**. In Blender render, "no material" means that an object has a default grayish material. But in X3D, "no material" means that an object is *unlit* (not affected by the usual light calculation; "shadeless" in Blender terms). This is actually a useful optimization in X3D in some cases, since unlit geometry can be rendered much faster.

    If you want your shape to be affected by lighting, just create a new material on your object, do not leave it without any materials.

    To easily assign a material to all the selected objects, use in Blender <i>Ctrl + L</i> and choose <i>Materials</i>.

    When creating a material, consider also changing the diffuse color to white, as the default is grayish (0.8,0.8,0.8). Consider also darkening the specular color, as the default Blender specular is very bright, which results in a plastic/polished/artificial look, which is often *not* what you want.

* The **material transparency (in resulting X3D file) is calculated as 1.0 - material alpha (in Blender model)**. That is, for X3D exporter, the *"alpha"* means *"how much is this material opaque"*. `Alpha = 0` means that the material is fully transparent, `alpha = 1` means that it is opaque. It will be multiplied with the *texture alpha* when rendering with blending. This is probably what you expect -- this is consistent with how X3D, and most 3D software treats the term "alpha".

    However, note that the Blender's treatment of the "alpha" (if you assign textures to your material) is more convoluted and a little counter-intuitive. In particular, **if you set `alpha = 1`, you will not see the texture alpha channel correctly in the Blender 3D view ("Texture" mode), and there seems to be no solution for this**. Just rest assured that the exported X3D will show the texture alpha correctly (which you can verify, by exporting and opening it e.g. with view3dscene).

    The Blender's internal renderer, and the OpenGL rendering, seem to calculate the "opaqueness" by this equation: `opaqueness =  (1-texture_alpha) * material_alpha + texture_alpha * dvar`. (See also Michalis' Blender lecture on http://michalis.ii.uni.wroc.pl/~michalis/teaching/blender/ ). So when `dvar = 1` (default) and `material_alpha = 1` (opaque), you have `opaqueness = 1`, so it's always opaque, regardless of the `texture_alpha`. You need to set `material_alpha = 0` and `dvar = 1` to see the texture alpha channel in Blender... Which is very counter-intuitive, and the Blender's own X3D exporter has a different understanding how the "alpha" works.

    Using the _Face Textures_ and _Face Textures Alpha_ (without assigning textures to material) works OK, then the alpha treatment is intuitive and consistent with X3D.

<!-- mentioned later:  * If you use our custom X3D exporter, **the Shadeless checkbox at material is supported perfectly**. It is exported to what we call a *purely emissive material*, which is never lit, and it has an optimized rendering throughout the Castle Game Engine. -->

## Textures

* To successfully **export the UV textures**, you should either select the *"Face Textures"* option at the material, or add the appropriate texture (with *Image or Movie* type, and with correct *UV* chosen) to the material. The second option is a little more work (you will need to create multiple materials if you use multiple textures on the same mesh), but it's also better (different material settings for each texture allow additional tweaking, e.g. multiplying it with color). 

* I advice to **set <tt>Path Mode</tt> as <tt>Relative</tt>** at the export dialog. The default state <tt>Path Mode = AUTO</tt> means that Blender tries to guess whether a relative or an absolute filename is best, and also records a couple of alternatives as URLs. Instead I advice to always use (only) relative paths, as then 
    1. your models work on other people's systems, 
    2. your models don't contain absolute paths specific to your system (which is often surprising, and causes unnecessary differences in the version control systems).

## Extra material textures (normal maps, specular maps) with CommonSurfaceShader

If you check the *Use CommonSurfaceShader* checkbox at exporting (available only in [our modified Blender exporter for now](https://castle-engine.io/creating_data_blender.php)), then the X3D file will contain a [CommonSurfaceShader](https://castle-engine.io/x3d_implementation_texturing_extensions.php#section_ext_common_surface_shader) describing some advanced material properties. This enables to smoothly use normal maps, specular maps and more from *Blender* to *Castle Game Engine*.

You should set up the textures in Blender *"texture slots"* to influence the appropriate shading parameters, and they will be automatically used by the exporter.

The following checkboxes in the *"Influence"* section of the *"Texture"* tab (in the Blender *"Properties"* window) are recognized and exported as proper CommonSurfaceShader fields:

* *Diffuse->Color* (RGB)
* *Diffuse->Alpha* (uses alpha channel of the provided texture)
* *Specular->Color* (RGB)
* *Specular->Hardness* (X3D uses only the alpha channel of the provided texture)
* *Shading->Ambient* (RGB)
* *Geometry->Normal* (RGB, converted to normal vectors in tangent space)
* *Geometry->Displace* (for now the texture itself is not used by CGE (it is assumed to be in the alpha channel of a *normal map* texture), but the *amount* of displacement is used)

Any combination of these parameters should work.

Notes:

* *About diffuse*: The *Castle Game Engine* right now always uses the *"diffuse texture"* as a combined *"diffuse + alpha"* texture. So, for future compatibiilty, always select both *"Diffuse->Color"* and *"Diffuse->Alpha"* checkboxes in Blender. This is a limitation of CGE, *not* a limitation of CommonSurfaceShader, X3D, or this Blender exporter.

* The amount of *"Influence"* (controlled by the sliders for each of the checkbox in the *"Influence"* section) does not matter. It is always treated as equal to 1.0 by the *Castle Game Engine*. This is for performance reasons (implementing an adjustable *"Influence"* amount, like Blender renderer allows, would cost us rendering time). Instead, you can edit the appropriate texture, or adjust the appropriate material parameter. *The one exception to this rule is the displacement*: The amount of displacement matters, it sets the base `displacementFactor` and `normalTextureParallaxHeight` that affect how the displacement is interpreted.

* The *material parameters are multiplied by the texture values*. So changing the material parameters still makes sense. If it seems like your texture has no effect, make sure that the appropriate material parameter is not zero. E.g. a *specular map* (texture affecting the *specular color*) will have no effect if on the "Material" tab in Blender you set *specular color* as zero (black).

* *About displacement*: The *Castle Game Engine* right now automatically detects and uses the *displacement* by looking at the alpha channel of your normalmap. Selecting the checkbox *"Geometry->Displace"* is only useful for now to set the *parallax height* for *parallax bump mapping*, it doesn't matter what image is assigned there (for now).