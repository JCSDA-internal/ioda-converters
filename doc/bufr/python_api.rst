Python
======

It is very easy query BUFR files via python.

.. code-block:: python
    .. example::
        import bufr

        # Make the QuerySet for all the data we want
        q = bufr.QuerySet()
        q.add('latitude', '*/CLAT')
        q.add('longitude', '*/CLON')
        q.add('radiance', '*/BRIT/TMBR')

        # Open the BUFR file and execute the QuerySet
        with bufr.File( './testinput/gdas.t00z.1bhrs4.tm00.bufr_d') as f:
            r = f.execute(q)

        # Use the ResultSet returned to get correctly dimensioned numpy arrays of the data
        lat = r.get('latitude')
        lon = r.get('longitude')
        rad = r.get('radiance')


The steps are:
#. Import bufr
#. Create a QuerySet
#. Open the bufr file (using the with statement)
#. Execute the QuerySet
#. Use the ResultSet to get the data

Create a QuerySet
-----------------

The QuerySet is a list of queries that you want to execute on the BUFR file. To create one, just
create an instance of the QuerySet class and then add queries to it using the `add` method. Each
item in the QuerySet consists of a name and the corresponding query path. The name is used to
retrieve the data from the ResultSet. It can be anything you want! The path can be any query path
described in :ref: `Query Path <query_path>`.

If you are only interested in specific subsets within the BUFR file you can instantiate the QuerySet
with a list of the Subsets you want. For example:

.. code-block:: python
    .. example::
        # Make the QuerySet for all the data we want
        q = bufr.QuerySet(['NC000001', 'NC000002'])
        q.add('latitude', '*/CLAT')
        q.add('longitude', '*/CLON')
        q.add('radiance', '*/BRIT/TMBR')

        # And so on...


Execute the QuerySet
--------------------

Just open the BUFR file and run execute on on the File object with the query set. It will run
through the entire BUFR file and return a ResultSet object.


Use the ResultSet
-----------------

Internally the ResultSet contains data structures which allow it to construct the numpy array data
sets using the keys defined in the QuerySet. To get the data, just use the `get` method.

It is also possible to group data elements with respect to each other. In this case call `get` with
the field you want to group by. So for example:

.. code-block:: python
    .. example::
        lat_grouped = r.get('latitude', group_by='radiance')
        lon_grouped = r.get('longitude', group_by='radiance')
        rad_grouped = r.get('radiance', group_by='radiance')

The result in either case are masked numpy arrays.
