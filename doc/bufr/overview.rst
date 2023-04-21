.. _overview:

What is BUFR?
=============

BUFR is a binary format for storing meteorological data. It is used by the World Meteorological
Organization (WMO) and the National Centers for Environmental Prediction (NCEP) to store
observations from weather stations, satellites, and other sources.

You can think of a BUFR file as a collection of messages where each message may originate from the
same or different sources at different times. Each message contains a table and a list data elements
called **subsets**. Each subset is associated with a standard ID like **NC000001**.

The table part of the message describes how the bytes are layed out in the data section for each
subset type (aka. Table A mnemonic). It takes the form of a tree with nested branches than can be
repeated in arbitrary ways. The traditional (NCEPLibs-bufr) way of illustrating this is as follows:

.. image:: images/NCEPLIB-table.png

Basically this is the output you get when you run the NCEPLIB_bufr gettab utility which gives you
an overview of the subset tables you may find in a BUFR tfile. In this case the BUFR file contains
one type of subset, **NC003010**.

