(in-package #:org.shirakumo.sf3)

;; toolkit.lisp
(docs:define-docs
  (function unix-to-universal-time
    "Convert a UNIX timestamp to a Common Lisp universal-time timestamp")

  (function universal-to-unix-time
    "Convert a Common Lisp universal-time timestamp to a UNIX timestamp"))

;; archive.lisp
(docs:define-docs
  (type archive-meta-entry
    "Representation of an archive file metadata entry.

See MAKE-ARCHIVE-META-ENTRY
See MODIFICATION-TIME
See CHECKSUM
See MIME-TYPE
See PATH")
  
  (function make-archive-meta-entry
    "Create an archive metadata entry.

See ARCHIVE-META-ENTRY")
  
  (type archive
    "Representation of an archive file.

See MAKE-ARCHIVE
See META-ENTRIES
See FILES
See ADD-FILE
See EXTRACT-FILE
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-archive
    "Create a new file archive

FILES should be a list of file specs, where a file spec is

  (FILE ...args)
  FILE

and will be passed to ADD-FILE, filling in the ARCHIVE argument.

See ARCHIVE (type)
See ADD-FILE")
  
  (function modification-time
    "Access the modification time of the metadata entry.

The modification time is a universal-time timestamp.

See ARCHIVE-META-ENTRY (type)")
  
  (function checksum
    "Access the CRC32 checksum.

See ARCHIVE-META-ENTRY (type)
See SF3-FILE-HEADER (type)")
  
  (function mime-type
    "Access the media mime-type.

You can also pass any SF3 file structure or its type to this to get
its intended mime-type back.

See ARCHIVE-META-ENTRY (type)
See ARCHIVE (type)
See AUDIO (type)
See IMAGE (type)
See LOG (type)
See MODEL (type)
See PHYSICS-MODEL (type)
See TABLE (type)
See TEXT (type)
See VECTOR-GRAPHIC (type)")
  
  (function path
    "Access the path of the file.

This is a string describing a relative UNIX namestring for the related
file.

See ARCHIVE-META-ENTRY (type)")
  
  (function files
    "Access the vector of file payloads.

File payloads are stored as octet vectors.
In order to get the associated file metadata, look at the
ARCHIVE-META-ENTRY of the same index in the META-ENTRIES vector.

See ARCHIVE (type)
See META-ENTRIES")

  (function meta-entries
    "Access the vector of archive metadata entries.

The metadata entries give you information about a file's path, type,
and so on.

See ARCHIVE (type)
See ARCHIVE-META-ENTRY (type)
See FILES")
  
  (function add-file
    "Add a new file to the archive.

Returns the modified ARCHIVE.

The FILE may be an octet-vector, string, or a pathname. In the former
two cases you must also pass the PATH keyword argument. In all cases
you may also specify the MIME-TYPE and MODIFICATION-TIME. If you do
not, they are determined automatically.

See FILES
See META-ENTRIES
See ARCHIVE (type)
See ARCHIVE-META-ENTRY (type)")
  
  (function extract-file
    "Extract a file from the archive to disk.

FILE may either be the path of the file to extract, the index of the
file, or T for all files.

If VERIFY is true, the resulting file on disk is checked against the
stored CRC-32 checksum.

If PRESERVE-MODIFICATION-TIME is true, the resulting file will have
the modification time as stored in the archive.

See FILES
See META-ENTRIES
See ARCHIVE (type)
See ARCHIVE-META-ENTRY (type)"))

;; audio.lisp
(docs:define-docs
  (type audio
    "Representation of an audio file.

See MAKE-AUDIO
See DURATION
See SAMPLERATE
See CHANNELS
See CHANNEL-LAYOUT
See SAMPLE-FORMAT
See SAMPLES
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-audio
    "Create a new audio file.

SAMPLES should be a vector with raw audio samples. The sample format
is inferred from the element-type of the vector. As such, you must
ensure the array is of the appropriate format. The array must also
have a multiple of CHANNELS elements.

See AUDIO (type)")
  
  (function duration
    "Returns the duration of the audio data contained in seconds as a float.

See AUDIO (type)")
  
  (function samplerate
    "Accesses the samplerate of the audio file in Hertz.

See AUDIO (type)")
  
  (function channels
    "Accesses the channel information of the file.

For audio files, this is the channel count.
For image files, this is the pixel format identifier.

See PIXEL-TYPE
See CHANNEL-LAYOUT
See IMAGE (type)
See AUDIO (type)")

  (function channel-layout
    "Returns the channel layout of the audio file as a list.

The list contains the following channel position identifiers:
  :CENTER-FRONT
  :LEFT-FRONT
  :RIGHT-FRONT
  :LEFT-REAR
  :RIGHT-REAR
  :SUBWOOFER
  :LEFT-SIDE
  :RIGHT-SIDE
  :CENTER-REAR

See CHANNELS
See AUDIO (type)")
  
  (function sample-format
    "Accesses the sample format type of the audio file.

May be one of:
  :SINT8
  :SINT16
  :SINT32
  :SINT64
  :UINT8
  :UINT16
  :UINT32
  :UINT64
  :FLOAT16
  :FLOAT32
  :FLOAT64

This will also correspond to the element-type of the SAMPLES array,
except for :FLOAT16, which is encoded as (unsigned-byte 16) due to
lack of support for short-floats in most implementations.

See AUDIO (type)")
  
  (function samples
    "Accesses the sample data of the audio file.

This is a raw PCM data vector structured as audio frames, interleaving
the channels in each frame according to the layout in CHANNEL-LAYOUT.

See AUDIO (type)"))

;; image.lisp
(docs:define-docs
  (type image
    "Representation of an image file.

See WIDTH
See HEIGHT
See DEPTH
See CHANNELS
See PIXEL-FORMAT
See PIXEL-TYPE
See PIXEL-STRIDE
See PIXELS
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-image
    "Create a new image file.

PIXELS should be a vector with raw pixel data. The pixel format
is inferred from the element-type of the vector. As such, you must
ensure the array is of the appropriate format. The array must also
have a multiple of WIDTH*HEIGHT*DEPTH*CHANNELS elements.

See PIXEL-TYPE
See IMAGE (type)")
  
  (function pixel-type
    "Returns the pixel type of the image file.

The pixel type can be one of:

  :V
  :VA
  :RGB
  :RGBA
  :AV
  :BGR
  :ABGR
  :ARGB
  :BGRA
  :CMYK
  :KYMC

Which describes the color channel purpose and arrangement within the
pixel data.

See IMAGE (type)")

  (function pixel-format
    "Accesses the format of the pixel channel elements.

This corresponds to the pixel data vector element-type.

See PIXELS
See IMAGE (type)")

  (function pixel-stride
    "Returns the number of elements that constitute a pixel.

See IMAGE (type)")
  
  (function width
    "Accesses the width of the object.

See IMAGE (type)
See VECTOR-GRAPHIC (type)
See ELLIPSOID (type)
See BOX (type)
See SIZE (type)")
  
  (function height
    "Accesses the height of the object.

See IMAGE (type)
See VECTOR-GRAPHIC (type)
See ELLIPSOID (type)
See BOX (type)
See SIZE (type)
See PILL (type)
See CYLINDER (type)")
  
  (function depth
    "Accesses the depth of the object.

See IMAGE (type)
See ELLIPSOID (type)
See BOX (type)")
  
  (function pixels
    "Accesses the pixel data of the image.

The vector contains the pixel data in \"row-major\" order, with
channel data interleaved. Meaning to access a specific channel value
the index is computed as:

  channel + PIXEL-STRIDE * (x + WIDTH * (y + HEIGHT * d))

See PIXEL-STRIDE
See WIDTH
See HEIGHT
See IMAGE (type)"))

;; log.lisp
(docs:define-docs
  (type log-entry
    "Representation of a single entry in a log file.

See SIZE
See TIME
See SEVERITY
See SOURCE
See CATEGORY
See MESSAGE
See MAKE-LOG-ENTRY")
  
  (function make-log-entry
    "Create a new log entry.

The size of the entry is automatically computed based on the other
attributes. The TIME is given in UNIVERSAL-TIME format.

See LOG-ENTRY (type)")
  
  (type log-chunk
    "Representation of a chunk of log entries in a log file.

See MAKE-LOG-CHUNK
See ENTRIES")
  
  (function make-log-chunk
    "Create a new log chunk.

The ENTRIES count is the number of entries that the chunk will be
capable of storing.

See LOG-CHUNK (type)")
  
  (type log
    "Representation of a log file.

See MAKE-LOG
See START-TIME
See END-TIME
See CHUNKS
See LOG-APPEND-CHUNK
See LOG-APPEND-ENTRY-EXTEND
See LOG
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-log
    "Create a new log instance.

The START-TIME and END-TIME should be given as a UNIVERSAL-TIME
timestamp and are converted to UNIX time for you. If END-TIME is not
given, the time is set to the maximal possible value, indicating that
the log file is not yet complete.

ENTRIES specifies the number of entries to allocate for in the first
chunk of the log file.

See MAKE-LOG-CHUNK
See END-TIME
See LOG (type)")
  
  (function size
    "Accesses the precomputed octet-size of the structure.

In the case of the COLUMN-SPEC, this represents the octet-size of the
data in this column rather than the size of the COLUMN-SPEC itself.

See LOG-ENTRY (type)
See LOG-CHUNK (type)
See COLUMN-SPEC (type)")
  
  (function time
    "Accesses the time at which the entry was created.

This is a UNIX timestamp.

See UNIX-TO-UNIVERSAL-TIME
See UNIVERSAL-TO-UNIX-TIME
See LOG-ENTRY (type)")
  
  (function severity
    "Accesses the severity number of the entry.

The more positive the number, the more severe, the more negative the
more detailed it is meant to be, with 0 being treated as \"neutral\".

See LOG-ENTRY (type)")
  
  (function source
    "Access the source indicator of the entry.

This is a string at most 255 characters long.

See LOG-ENTRY (type)")
  
  (function category
    "Access the category of the entry.

This is a string at most 255 characters long.

See LOG-ENTRY (type)")
  
  (function message
    "Access the message of the entry.

This is mostly free-form, application-specific text about the log
event.

See LOG-ENTRY (type)")
  
  (function entries
    "Access the vector of log-entries in the chunk.

See LOG-ENTRY (type)
See LOG-CHUNK (type)")
  
  (function start-time
    "Access the starting time of the log.

This is a UNIX timestamp.

See UNIX-TO-UNIVERSAL-TIME
See UNIVERSAL-TO-UNIX-TIME
See LOG (type)")
  
  (function end-time
    "Access the ending time of the log.

This is a UNIX timestamp. If the timestamp is equivalent to the most
positive 63 bit number, the end time is considered to be
\"tentative\" and the log file unfinished.

See UNIX-TO-UNIVERSAL-TIME
See UNIVERSAL-TO-UNIX-TIME
See LOG (type)")
  
  (function chunks
    "Access the chunks vector of the log.

See LOG (type)
See LOG-CHUNK (type)
See LOG-APPEND-CHUNK")
  
  (function log-append-chunk
    "Append a new chunk to the log.

ARGS are passed on to MAKE-LOG-CHUNK.
The LOG is returned again.

See MAKE-LOG-CHUNK
See LOG (type)")
  
  (function log-append-entry-extend
    "Append a new entry to the log, creating a new chunk if necessary.

ARGS are passed on to MAKE-LOG-CHUNK.
The LOG is returned again.

See LOG-ENTRY (type)
See LOG (type)")

  (function log
    "Create a new log entry and append it.

This is the same as invoking MAKE-LOG-ENTRY and
LOG-APPEND-ENTRY-EXTEND.

See MAKE-LOG-ENTRY
See LOG-APPEND-ENTRY-EXTEND
See LOG (type)
See LOG-ENTRY (type)"))

;; model.lisp
(docs:define-docs
  (type model
    "Representation of a 3D model file.

See MAKE-MODEL
See VERTEX-ATTRIBUTES
See VERTEX-STRIDE
See TEXTURE-TYPES
See TEXTURES
See FACES
See VERTICES
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-model
    "Create a new 3D model instance.

FACES should be an (UNSIGNED-BYTE 32) array of vertex indices, and
VERTICES should be a SINGLE-FLOAT array of vertex attributes.

VERTEX-ATTRIBUTES should be the set of attributes as stored in
VERTICES. The attributes as stored in VERTICES MUST follow the order
of the attributes outlined in this set. The possible sets of
attributes can be seen in VERTEX-ATTRIBUTES.

MATERIAL should be a property list of relative paths to material
textures. The set of permissible keys can be seen in TEXTURE-TYPES.

See VERTEX-ATTRIBUTES
See TEXTURE-TYPES
See MODEL (type)")
  
  (function vertex-attributes
    "Returns the set of vertex attributes stored per vertex.

These sets are possible:

  (:position)
  (:position :uv)
  (:position :color)
  (:position :normal)
  (:position :uv :normal)
  (:position :color :normal)
  (:position :uv :normal :tangent)
  (:position :color :normal :tangent)

Each attribute corresponds to three float values in the vertices
array, except for :UV which corresponds to two.

See VERTEX-STRIDE
See MODEL (type)")
  
  (function vertex-stride
    "Returns the number of float values that constitute each vertex.

See VERTICES
See MODEL (type)")
  
  (function texture-types
    "Returns the set of textures that makes up the material.

The textures are stored in the order of the returned set.

See TEXTURES
See MODEL (type)")
  
  (function textures
    "Accesses the vector of paths for the material's textures.

See TEXTURE-TYPES
See MODEL (type)")
  
  (function faces
    "Accesses the vector of vertex indices that make up the faces.

See MODEL (type)")
  
  (function vertices
    "Access the vector of vertex attribute data.

In the case of a MESH this is only position data.

See VERTEX-ATTRIBUTES
See VERTEX-STRIDE
See MODEL (type)
See MESH (type)"))

;; physics-model.lisp
(docs:define-docs
  (type ellipsoid
    "Representation of an ellipsoid shape.

See MAKE-ELLIPSOID
See WIDTH
See HEIGHT
See DEPTH
See PHYSICS-MODEL (type)")
  
  (function make-ellipsoid
    "Create a new ellipsoid shape.

If the TRANSFORM is omitted, it is set to the identity matrix.
Otherwise it should be a 4x4 matrix in the form of a sequence of 16
single-floats.

See ELLIPSOID (type)
See SHAPE (type)")
  
  (type box
    "Representation of a box shape.

See MAKE-BOX
See WIDTH
See HEIGHT
See DEPTH
See PHYSICS-MODEL (type)")
  
  (function make-box
    "Create a new box shape.

If the TRANSFORM is omitted, it is set to the identity matrix.
Otherwise it should be a 4x4 matrix in the form of a sequence of 16
single-floats.

See ELLIPSOID (type)
See SHAPE (type)")
  
  (type cylinder
    "Representation of a cylindrical shape.

See MAKE-CYLINDER
See BOTTOM-RADIUS
See TOP-RADIUS
See HEIGHT
See PHYSICS-MODEL (type)")
  
  (function make-cylinder
    "Create a new cylinder shape.

If the TRANSFORM is omitted, it is set to the identity matrix.
Otherwise it should be a 4x4 matrix in the form of a sequence of 16
single-floats.

See ELLIPSOID (type)
See SHAPE (type)")
  
  (type pill
    "Representation of a pill-box shape.

See MAKE-PILL
See BOTTOM-RADIUS
See TOP-RADIUS
See HEIGHT
See PHYSICS-MODEL (type)")
  
  (function make-pill
    "Create a new pill shape.

If the TRANSFORM is omitted, it is set to the identity matrix.
Otherwise it should be a 4x4 matrix in the form of a sequence of 16
single-floats.

See ELLIPSOID (type)
See SHAPE (type)")
  
  (type mesh
    "Representation of a convex mesh shape.

See MAKE-MESH
See VERTICES
See PHYSICS-MODEL (type)")
  
  (function make-mesh
    "Create a new mesh shape.

VERTICES should be a sequence of single-floats, three of which each
describe a vertex' position. The vertices should collectively form a
convex hull.

If the TRANSFORM is omitted, it is set to the identity matrix.
Otherwise it should be a 4x4 matrix in the form of a sequence of 16
single-floats.

See ELLIPSOID (type)
See SHAPE (type)")
  
  (type shape
    "Representation of an arbitrary physics shape.

You can also access the inner shape's attributes through this object
without needing to unpack the inner shape first.

See MAKE-SHAPE
See TRANSFORM
See DATA
See PHYSICS-MODEL (type)")
  
  (function make-shape
    "Create a new shape.

This is typically not necessary as the shape constructors individually
automatically wrap the shape data in a SHAPE instance.

See SHAPE (type)")
  
  (type physics-model
    "Representation of a physics model.

See MAKE-PHYSICS-MODEL
See MASS
See TENSOR
See SHAPES
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-physics-model
    "Create a new physics model.

The MASS should be expressed in Kg. The TENSOR should be a sequence of
9 single-floats, describing the overall inertia tensor of the entire
model.

SHAPES should be the SHAPE instances that make up the physics model's
hull.

See MAKE-ELLIPSOID
See MAKE-BOX
See MAKE-CYLINDER
See MAKE-PILL
See MAKE-MESH
See PHYSICS-MODEL (type)")
  
  (function bottom-radius
    "Accesses the bottom radius of the shape.

See CYLINDER (type)
See PILL (type)")
  
  (function top-radius
    "Accesses the top radius of the shape.

See CYLINDER (type)
See PILL (type)")
  
  (function transform
    "Accesses the transform matrix of the shape.

The transform matrix is a row-major 16-element single-float vector
that stores the 4x4 affine transformation matrix to position the shape
relative to origin of the entire model.

See SHAPE (type)
See PHYSICS-MODEL (type)")
  
  (function data
    "Accesses the inner data of the physics shape.

See SHAPE (type)")
  
  (function mass
    "Accesses the mass of the overall physics model in Kg.

See PHYSICS-MODEL (type)")
  
  (function tensor
    "Accesses the inertia tensor matrix of the physics shape.

The inertia tensor is a row-major 9-element single-float vector that
stores the 3x3 inertia tensor matrix.

See PHYSICS-MODEL (type)")
  
  (function shapes
    "Accesses the vector of shapes that make up the physics model.

See SHAPE (type)
See PHYSICS-MODEL (type)"))

;; table.lisp
(docs:define-docs
  (type column-spec
    "Representation of the specification for a column in a table.

See MAKE-COLUMN-SPEC
See NAME
See SIZE
See KIND
See COLUMN-SPEC-TYPE
See COLUMN-SPEC-ELEMENT-SIZE
See COLUMN-SPEC-ELEMENT-COUNT")
  
  (function make-column-spec
    "Create a new column specification.

LENGTH is automatically converted to the octet-size of a multiple of
the TYPE, except for if the TYPE is :STRING, in which case the LENGTH
is taken to be the maximum length of the string in octets.

See COLUMN-SPEC (type)
See COLUMN-SPEC-TYPE")
  
  (function kind
    "Accesses the kind of the type stored.

See COLUMN-SPEC-TYPE
See COLUMN-SPEC (type)
See SF3-FILE-HEADER (type)")

  (function column-spec-type
    "Returns the type of the column data.

This can be one of:
  :uint8
  :uint16
  :uint32
  :uint64
  :sint8
  :sint16
  :sint32
  :sint64
  :float16
  :float32
  :float64
  :string
  :timestamp
  :high-resolution-timestamp
  :boolean

See KIND
See COLUMN-SPEC (type)")
  
  (function column-spec-element-size
    "Returns the octet size of an element in the column.

See KIND
See COLUMN-SPEC (type)")
  
  (function column-spec-element-count
    "Returns the number of elements stored in the column.

See KIND
See COLUMN-SPEC-ELEMENT-SIZE
See SIZE
See COLUMN-SPEC (type)")
  
  (type table
    "Representation of a table.

See MAKE-TABLE
See COLUMN-COUNT
See ROW-COUNT
See COLUMN-SPECS
See ROW-DATA
See ROW
See CELL
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-table
    "Create a new table.

COLUMNS should be specifications of the columns in the table, where
each spec should be either a COLUMN-SPEC instance, a list of arguments
to MAKE-COLUMN-SPEC, or a string to pass to MAKE-COLUMN-SPEC.

The DATA should be a sequence of sequences, where each inner sequence
must have the same number of entries as COLUMNS. In cases where a
column can hold multiple elements, the cells for this column may
either be specified via a single element, or a list of elements.

See CELL
See MAKE-COLUMN-SPEC
See TABLE (type)")
  
  (function column-count
    "Accesses the number of columns in the table.

See TABLE (type)")
  
  (function row-count
    "Accesses the number of rows in the table.

See TABLE (type)")
  
  (function column-specs
    "Accesses the vector of column specifications in the table.

See COLUMN-SPEC (type)
See TABLE (type)")
  
  (function row-data
    "Accesses the raw, encoded row data.

The data is encoded as an octet vector.
In order to decode data, please use the ROW and CELL functions.

See ROW
See CELL
See TABLE (type)")
  
  (function row
    "Returns the specified row contents of the table as a list.

If a cell contains multiple elements, it is encoded as a list.

See TABLE (type)")
  
  (function cell
    "Accesses the specified cell contents of the table.

If the cell contains multiple elements, it is encoded as a list.
When setting a cell, you may similarly set a list of elements, or a
single element value.

See TABLE (type)"))

;; text.lisp
(docs:define-docs
  (type color-option
    "Representation of a color markup option.

See MAKE-COLOR-OPTION
See R
See G
See B
See MARKUP (type)")
  
  (function make-color-option
    "Create a new color markup option.

The values should be in the range of [0,1].

See COLOR-OPTION (type)")
  
  (type size-option
    "Representation of a font size markup option.

See MAKE-SIZE-OPTION
See FONT-SIZE")
  
  (function make-size-option
    "Create a new size markup option.

The size should be a positive number.

See SIZE-OPTION (type)")
  
  (type heading-option
    "Representation of a heading markup option.

See MAKE-HEADING-OPTION
See LEVEL")
  
  (function make-heading-option
    "Create a new heading markup option.

The level should be a positive integer.

See HEADING-OPTION (type)")
  
  (type link-option
    "Representation of a link markup option.

See ADDRESS
See MAKE-LINK-OPTION")
  
  (function make-link-option
    "Create a new hyperlink markup option.

The address should be a string naming the address of a corresponding
target-option, or an URL.

See TARGET-OPTION (type)
See LINK-OPTION (type)")
  
  (type target-option
    "Representation of a link target option.

See ADDRESS
See MAKE-TARGET-OPTION")
  
  (function make-target-option
    "Create a new link target markup option.

The address should be a string naming the target.

See TARGET-OPTION (type)
See LINK-OPTION (type)")

  (type font-option
    "Representation of a font change option.

See FONT
See MAKE-FONT-OPTION")

  (function make-font-option
    "Create a new font change markup option.

The font should be a string naming the font family to use.

See FONT-OPTION(type)")
  
  (type markup
    "Representation of a markup section.

See MAKE-MARKUP
See START
See END
See OPTION")
  
  (function make-markup
    "Create a new markup section.

The START and END should be indices for the range of text to mark up,
where the END defines the index after which the option no longer
applies.

START and END must be positive integers, and END must be greater or
equal to START.

OPTION should be a valid markup option type:

  :BOLD
  :ITALIC
  :UNDERLINE
  :STRIKE
  :MONO
  COLOR-OPTION
  SIZE-OPTION
  HEADING-OPTION
  LINK-OPTION
  TARGET-OPTION
  FONT-OPTION

See MARKUP (type)")
  
  (type text
    "Representation of a text with markup.

See MAKE-TEXT
See TEXT
See MARKUP
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-text
    "Create a new text instance.

TEXT should be a string to mark up.
MARKUP should be a number of markup specs, which can either be a
MARKUP instance, or a list composed of a START and END index and
either a valid markup option instance, or the name of an option and
the arguments for its corresponding constructor.

For example (0 1 :COLOR 1 0 0) or simply (0 1 :BOLD).

See TEXT (type)")
  
  (function r
    "Accesses the red component of the color.

See COLOR-OPTION (type)
See COLOR (type)")
  
  (function g
    "Accesses the green component of the color.

See COLOR-OPTION (type)
See COLOR (type)")
  
  (function b
    "Accesses the blue component of the color.

See COLOR-OPTION (type)
See COLOR (type)")
  
  (function level
    "Accesses the heading level.

See HEADING-OPTION (type)")
  
  (function address
    "Accesses the address.

See LINK-OPTION (type)
See TARGET-OPTION (type)")
  
  (function text
    "Accesses the string text of the element.

See TEXT (type)
See TEXT-SHAPE (type)"))

;; vector-graphic.lisp
(docs:define-docs
  (type color
    "Representation of a color.

See R
See G
See B
See A
See MAKE-COLOR")
  
  (function make-color
    "Create a new color instance.

R G B and A should be numbers in the range [0,1] to indicate the value
of the corresponding color channel.

See COLOR (type)")
  
  (type point
    "Representation of a position on the canvas.

See X
See Y
See MAKE-POINT")
  
  (function make-point
    "Create a new point instance.

X and Y should be coordinates of the position.

See POINT (type)")

  (type size
    "Representation of an extent on the canvas.

See W
See H
See MAKE-SIZE")
  
  (function make-size
    "Create a new size instance.

W and H should be positive numbers representing the dimensions of the
extent.

See SIZE (type)")
  
  (type shape-fill
    "Representation of the shape fill parameters.

See FILL-COLOR
See OUTLINE-COLOR
See OUTLINE-THICKNESS
See MAKE-SHAPE-FILL")
  
  (function make-shape-fill
    "Create a new shape-fill instance.

FILL-COLOR and OUTLINE-COLOR should be COLOR instances.
OUTLINE-THICKNESS should be a positive number representing the
thickness of the outline in pixels.

See SHAPE-FILL (type)")
  
  (type shape-bounds
    "Representation of the bounds of a shape.

The POINT defines the lower left corner, and the SIZE the extent of
the shape.

See POINT
See SIZE
See MAKE-SHAPE-BOUNDS")
  
  (function make-shape-bounds
    "Create a new shape-bounds instance.

X and Y should be the lower left corner coordinates, and W and H the
dimensions of the extent.

See SHAPE-BOUNDS (type)")
  
  (type line
    "Representation of a segmented line.

See POINTS
See COLOR
See THICKNESS
See MAKE-LINE")
  
  (function make-line
    "Create a new line instance.

POINTS should be a sequence of POINT instances.
THICKNESS should be a positive number representing the thickness of
the line in pixels.
COLOR should be a COLOR instance describing the color of the line.

See LINE (type)")
  
  (type rectangle
    "Representation of a rectangle.

See BOUNDS
See FILL
See MAKE-RECTANGLE")
  
  (function make-rectangle
    "Create a new rectangle instance.

X and Y should be the lower left corner coordinates, and W and H the
dimensions of the rectangle.
FILL-ARGS are passed verbatim to MAKE-SHAPE-FILL

See MAKE-SHAPE-FILL
See RECTANGLE (type)")
  
  (type circle
    "Representation of a circle.

See BOUNDS
See FILL
See MAKE-CIRCLE")
  
  (function make-circle
    "Create a new circle instance.

X and Y should be the lower left corner coordinates, and W and H the
dimensions of the circle.
FILL-ARGS are passed verbatim to MAKE-SHAPE-FILL

See MAKE-SHAPE-FILL
See CIRCLE (type)")
  
  (type polygon
    "Representation of a closed polygon.

See POINTS
See FILL
See MAKE-POLYGON")
  
  (function make-polygon
    "Create a new polygon instance.

POINTS should be a sequence of POINT instances defining the edges of
the outline of the polygon.
FILL-ARGS are passed verbatim to MAKE-SHAPE-FILL

See MAKE-SHAPE-FILL
See POLYGON (type)")
  
  (type curve
    "Representation of a shape with a Bezier curve outline.

See POINTS
See FILL
See MAKE-CURVE")
  
  (function make-curve
    "Create a new curve instance.

POINTS should be a sequence of POINT instances defining the Bezier
curve segments of the outline. Each outline segment is made up of four
points, the first and last being the edge points and the second and
third being the corresponding control points. For successive segments
the first and final edge point are shared as no gaps between segments
are allowed anyway.
FILL-ARGS are passed verbatim to MAKE-SHAPE-FILL

See MAKE-SHAPE-FILL
See CURVE (type)")
  
  (type text-shape
    "Representation of a text shape.

See POINT
See COLOR
See FONT
See FONT-SIZE
See TEXT
See MAKE-TEXT-SHAPE")
  
  (function make-text-shape
    "Create a new text-shape instance.

X and Y define the point in the middle of the baseline of the first
character in the TEXT. COLOR should be a COLOR instance to define the
\"foreground\" color of the text. FONT should be a string identifier
of the font family to use for the text. FONT-SIZE should be a positive
number defining the height of a text line in pixels.

See TEXT-SHAPE (type)")
  
  (type vector-graphic
    "Representation of a vector graphic image.

See WIDTH
See HEIGHT
See INSTRUCTIONS
See MAKE-VECTOR-GRAPHIC
See WRITE-SF3
See READ-SF3
See TELL-SF3")
  
  (function make-vector-graphic
    "Create a new vector graphic image.

WIDTH and HEIGHT should be the dimensions of the image canvas.
INSTRUCTIONS should be a sequence of drawing instructions, which may
be one of the following:

  LINE
  RECTANGLE
  CIRCLE
  POLYGON
  CURVE
  TEXT-SHAPE
  (ARRAY SINGLE-FLOAT (6))

In the array case it represents a 3x2 transform matrix to be applied.
to all following instructions until the next transform matrix.

See VECTOR-GRAPHIC (type)")
  
  (function a
    "Accesses the alpha channel of the color.

See COLOR (type)")
  
  (function x
    "Accesses the X position of the point.

See POINT (type)")
  
  (function y
    "Accesses the Y position of the point.

See POINT (type)")
  
  (function w
    "Accesses the WIDTH extent of the size.

See SIZE (type)")
  
  (function h
    "Accesses the HEIGHT extent of the size.

See SIZE (type)")

  (function point
    "Accesses the lower left corner point of the extent.

See SHAPE-BOUNDS (type)")

  (function size
    "Accesses the dimensions of the extent.

See SHAPE-BOUNDS (type)")
  
  (function fill-color
    "Accesses the fill color of the fill specification.

See SHAPE-FILL (type)")
  
  (function outline-color
    "Accesses the outline color of the fill specification.

See SHAPE-FILL (type)")
  
  (function outline-thickness
    "Accesses the outline thickness of the fill specification.

See SHAPE-FILL (type)")
  
  (function thickness
    "Accesses the thickness of the line.

See LINE (type)")
  
  (function bounds
    "Accesses the bounds of the shape.

See SHAPE-BOUNDS (type)
See RECTANGLE (type)
See CIRCLE (type)")
  
  (function fill
    "Accesses the fill description of the shape.

See SHAPE-FILL (type)
See CURVE (type)
See POLYGON (type)
See RECTANGLE (type)
See CIRCLE (type)")
  
  (function points
    "Accesses the vector of outline points of the shape.

See POLYGON (type)
See CURVE (type)
See POINT (type)")
  
  (function font
    "Accesses the font-family descriptor of the text.

See FONT-OPTION (type)
See TEXT-SHAPE (type)")
  
  (function font-size
    "Accesses the font-size of the object.

The size describes the height of a line of text from its baseline in
pixels.

See TEXT-SHAPE (type)
See SIZE-OPTION (type)")
  
  (function instructions
    "Accesses the vector of drawing instructions that make up the image.

The elements may be instances of the following types:

  LINE
  RECTANGLE
  CIRCLE
  POLYGON
  CURVE
  TEXT-SHAPE
  (ARRAY SINGLE-FLOAT (6))

See VECTOR-GRAPHIC (type)"))

;; sf3.lisp
(docs:define-docs
  (type sf3-file-header
    "Representation of the generic header of an SF3 file.

See MAKE-SF3-FILE-HEADER
See KIND
See CHECKSUM")
  
  (function make-sf3-file-header
    "Create a new SF3-FILE-HEADER instance.

The KIND should name the type of file format stored.

See SF3-FILE-HEADER (type)")
  
  (function file-extension
    "Returns the file extension to be used with the given object or format type.

See ARCHIVE (type)
See AUDIO (type)
See IMAGE (type)
See LOG (type)
See MODEL (type)
See PHYSICS-MODEL (type)
See TABLE (type)
See TEXT (type)
See VECTOR-GRAPHIC (type)")
  
  (function read-sf3
    "Read an SF3 file from the given storage.

Depending on the storage backend, additional arguments may be
necessary. The following backends are supported:

  PATHNAME
  STREAM
  OCTET-VECTOR (optionally accepts :START and :END key arguments)
  FOREIGN-POINTER (requires a SIZE argument)

If successful returns the format instance.
If parsing fails, an error is signalled.

See TELL-SF3
See ARCHIVE (type)
See AUDIO (type)
See IMAGE (type)
See LOG (type)
See MODEL (type)
See PHYSICS-MODEL (type)
See TABLE (type)
See TEXT (type)
See VECTOR-GRAPHIC (type)")
  
  (function write-sf3
    "Write an SF3 file to the given storage.

Depending on the storage backend, additional arguments may be
necessary. The following backends are supported:

  PATHNAME
  STREAM
  OCTET-VECTOR (optionally accepts :START and :END key arguments)
  FOREIGN-POINTER (requires a SIZE argument)

If successful returns the storage.
If serialisation fails, an error is signalled.

See ARCHIVE (type)
See AUDIO (type)
See IMAGE (type)
See LOG (type)
See MODEL (type)
See PHYSICS-MODEL (type)
See TABLE (type)
See TEXT (type)
See VECTOR-GRAPHIC (type)")
  
  (function tell-sf3
    "Determine the type of SF3 file within the storage.

Depending on the storage backend, additional arguments may be
necessary. The following backends are supported:

  PATHNAME
  STREAM
  OCTET-VECTOR (optionally accepts :START and :END key arguments)
  FOREIGN-POINTER (requires a SIZE argument)

If successful returns the type of file stored.
If parsing of even the SF3 header fails, an error is signalled.

See READ-SF3
See ARCHIVE (type)
See AUDIO (type)
See IMAGE (type)
See LOG (type)
See MODEL (type)
See PHYSICS-MODEL (type)
See TABLE (type)
See TEXT (type)
See VECTOR-GRAPHIC (type)"))
