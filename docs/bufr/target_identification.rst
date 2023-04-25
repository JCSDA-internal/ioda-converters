.. _target_identification:

Target Identification
=====================

What is a target?
_________________
A target is a unique leaf element (or node) which is described in the BUFR message
table "tree" (see :ref:`BUFR Overview <overview>`). In order to uniquely describe this node you can
think of it as describing the path through the "tree" starting from the root going all the way to
the specific leaf that is needed. The path identifies the targeted leaf element uniquely.

Finding a Target?
_________________

First identify the data elements you want. You can consult the first section in the data provided by
the gettab utility to figure out which specific mnemonics you want within which sequences (you may
have the same mnemonic string mentioned in different sequences but you have to figure out which one
you actually want).

There are generally two ways to do this:

Using gettab
~~~~~~~~~~~~

The first way involves using the gettab NCEPLIB-bufr utility gettab to print out the BUFR meta data
and then trace through the repeated sequences until you get to the data element that you want.




