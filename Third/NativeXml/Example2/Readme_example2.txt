XmlEditor.exe 

Version: v2.1
Date:    11May2006

Author:  N. Haeck M.Sc.
         n.haeck@simdesign.nl

Copyright (c) 2001 - 2006 by N. Haeck


Features
========

Opening XML Files

Click on File->Open to select an XML file to analyse. Error messages will be displayed in the status bar if the file is not a valid XML file. If the file is valid, the status bar will show version, encoding and comments.

Saving XML Files

You can save any changes to the XML file by selecting File->Save. Select a valid filename in the File Save dialog box. If you do not select an extension then ".xml" is automatically attached.

XML Tree

This tab-page shows the XML nodes and their subnodes, as well as their values. You can browse the tree with your mouse or with the arrow keys.

XML Source

Click on this tab-page to see the source of the XML document in readable format

Attributes

This tab-page shows the attributes of each XML node. If you select "Single elements as attributes" then also the single elements will be shown here. You can distinguish between the two by their icon.

Options:

Single elements as attributes
Use this option to show the single elements (elements that have no sub-elements, and no attributes, only a value) in the  attribute list.

Hide single elements
Use this option to hide the single elements from the tree-list. Use together with "single elements as attributes".

Readable names for nodes
If checked, the names of nodes will not be displayed as capitals but as one capital followed by lowercase characters.

Compiling the example
=====================

If you want to compile this example you need:

1) Place NativeXml.dcu into the project folder
2) Install TVirtualTreeview (by Mike Liscke)

Then open the project file XmlEditor.dpr

Version History
===============

v2.1 (11May2006)

- Adapted for NativeXml
- Editing capabilities

v2.0 (30jul2003)

- Adapted to XmlDocuments.pas

v1.0 (16jul2001)

- Initial release of testing software NHXML.EXE for XML Files
