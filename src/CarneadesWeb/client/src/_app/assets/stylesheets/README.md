======================
CarneadesWeb: Stylesheet guide
======================
The aim of this document is to explain the folder structure. For further details about Sass see [sass-lang.com](http://sass-lang.com/). Furthermore, this files are compiled with Compass. For details about Compass see [compass-style.org](http://compass-style.org/). Compass is executed via grunt.
For example, if you wnat to compile the css files only and update the dist folder then use:
grunt compass
grunt cssmin

Regarding the topic of how to structure a sass project, I recommend reading [thesassway.com/beginner/how-to-structure-a-sass-project](http://thesassway.com/beginner/how-to-structure-a-sass-project). This articles provides you with valuable information and illustrates the basic concept. However, for the sake of practicability, we slightly changed the structure in our approach.

# Folder structure

## Layout
Contains small adjustments to the bootstrap grid layout.

## Projects

### Modules
The modules directory is reserved for Sass code that doesn't cause Sass to actually output CSS. Things like mixin declarations, functions, and variables.

### Vendor
The vendor directory is for third-party CSS. This is handy when using prepackaged components developed by other people (or for your own components that are maintained in another project). jQuery UI and a color picker are examples of CSS that you might want to place in the vendor directory. As a general rule I make it a point not to modify files in my vendor directory. If I need to make modifications I add those after the vendored files are included in my primary stylesheet. This should make it easy for me to update my third-party stylesheets to more current versions in the future.
So far, this folder contains bootstrap-scss.

## Partials
The partials directory is where the meat of my CSS is constructed. A lot of folks like to break their stylesheets into header, content, sidebar, and footer components (and a few others). As I'm more of a SMACSS guy myself, I like to break things down into much finer categories (typography, buttons, textboxes, selectboxes, etc…).

### Markos
Markos project specific customization. Compiles to markos.css

### Copyright
Copyright project specific customization. Compiles to copyright.css

### Default
Default project specific customization. Compiles to default.css

# How to extend
Assuming you set up Carneades sucessfully and you are able to build the system, you can customize your project. I am going to illustrate the workflow based on the markos project. You can find the files relevant for the markos project in assets/stylesheets/projects/markos.
The folder contains the following files:
- _banner.scss: Contains all rules for the banner component
- _variables.scss: Bootstrap variables template customized by markos project. In this file you will set variables such as colors and layout(font, grid, etc.) settings.
- _footer.scss: Contains all rules for the footer component

In assets/stylesheets there is a file called markos.scss. This files contains the rules for generating the markos.css file.

If you open markos.scss, you see that it basically imports all other files such as the bootstrap components and the project specific files at the bottom. If you want to customize a bootstrap component, I recommend to copy the file into the markos folder and replace the official version in markos.scss with the new one. If you want to extend the markos css rules, then I recommend u create a new file in the markos project and import it in markos.scss.

If you want to include additional grafics, i.e. in a background then keep in mind that those theme related files must be located in the real project folders. The path is specified in ~/carneades.clj.
