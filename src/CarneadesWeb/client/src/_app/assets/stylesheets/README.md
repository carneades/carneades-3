======================
CarneadesWeb: Stylesheet guide
======================

# Folder structure: Modules, partials, and vendor

## Modules
The modules directory is reserved for Sass code that doesn't cause Sass to actually output CSS. Things like mixin declarations, functions, and variables.

## Partials
The partials directory is where the meat of my CSS is constructed. A lot of folks like to break their stylesheets into header, content, sidebar, and footer components (and a few others). As I'm more of a SMACSS guy myself, I like to break things down into much finer categories (typography, buttons, textboxes, selectboxes, etc…).

## Vendor
The vendor directory is for third-party CSS. This is handy when using prepackaged components developed by other people (or for your own components that are maintained in another project). jQuery UI and a color picker are examples of CSS that you might want to place in the vendor directory. As a general rule I make it a point not to modify files in my vendor directory. If I need to make modifications I add those after the vendored files are included in my primary stylesheet. This should make it easy for me to update my third-party stylesheets to more current versions in the future.
