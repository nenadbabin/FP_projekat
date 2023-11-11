# Photo Editor (Functional Programming course project)

The project involves creating a Scala-based software tool with a graphical user interface for processing and blending digital images. Key features of this tool include:

1. **Project Management**: Users can create, load, and save projects. The project file contains information about the images being processed, the layers containing these images, the operations applied, and supports standard image formats like JPG and PNG. A custom file format is also defined for serializing relevant data.

2. **Layer Management**: The tool allows working with layers, which behave like transparent sheets stacked from 1 to N, where 1 is the top layer. Each layer contains an image, and its visibility depends on the transparency of the layers above it. Users can select active layers for image operations and choose visible layers for the final blended image.

3. **Selection Tool**: Users can define one or more rectangular areas as a selection and name them. Selections can be active or inactive. Only active layers are affected during image operations. There's also an option to fill the selection with a specified color. Selections can be deleted, reverting the colored pixels to their original state.

4. **Image Operations**: Operations can be applied to selected pixels or the entire image and are independently applied to each of the three primary components (R, G, B). These operations include basic arithmetic operations (addition, subtraction, multiplication, division), functions like power, log, abs, min, max, and the creation of named composite functions by chaining these basic operations and functions.

5. **Advanced Functions**: The tool includes predefined functions like inversion, conversion to grayscale, filtering, median, and weighted average value calculations. Users can also create named sequences of operations.
