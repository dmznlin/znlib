Registered users only
=====================

This example does not use NativeXml, but uses NativeXmlAppend.pas.

With NativeXmlAppend.pas, you can add Xml nodes to the end of an Xml Document
that exists in a file on disk, WITHOUT having to read it in completely.

The code works by scanning backwards from the end of the file, and finding
a node to add data to.

It is taken from a "real-life" demo for one of our customers.

See Append.dpr for a demo.



