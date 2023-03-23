import bufr

if __name__ == '__main__':
    path = '/Users/rmclaren/Work/ioda-converters/test/testinput/gdas.t00z.1bhrs4.tm00.bufr_d'
    with bufr.File(path) as f:
        q = bufr.QuerySet()
        q.add('latitude', '*/CLON')
        q.add('longitude', '*/CLAT')
        q.add('radiance', '*/BRIT{1}/TMBR')
        q.add('radiance_all', '*/BRIT/TMBR')

    r = f.execute(q)

    lat = r.get('latitude')
    lon = r.get('longitude')
    radiance = r.get('radiance')

