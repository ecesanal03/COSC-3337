from tropycal import tracks

basin = tracks.TrackDataset(basin='north_atlantic',include_btk=False)

target_cities = {
    "New Orleans, USA": (29.9511, -90.0715),
    "Houston, USA": (29.7604, -95.3698),
    "Tampa, USA": (27.9506, -82.4572),
    "Miami, USA": (25.7617, -80.1918),
    "Corpus Christi, USA": (27.8006, -97.3964),
    "Pensacola, USA": (30.4213, -87.2169),
    "Mobile, USA": (30.6954, -88.0399),
    "Galveston, USA": (29.3013, -94.7977),
    "Biloxi, USA": (30.3960, -88.8853),
    "Key West, USA": (24.5551, -81.7800),
    "Veracruz, Mexico": (19.1738, -96.1342),
    "Tampico, Mexico": (22.2553, -97.8686),
    "Campeche, Mexico": (19.8453, -90.5235),
    "Cancún, Mexico": (21.1619, -86.8515),
    "Mérida, Mexico": (20.9674, -89.5926),
    "Ciudad del Carmen, Mexico": (18.6491, -91.8071),
    "Progreso, Mexico": (21.2836, -89.6645),
    "Coatzacoalcos, Mexico": (18.1489, -94.4202),
    "Tuxpan, Mexico": (20.9589, -97.4044),
    "Havana, Cuba": (23.1136, -82.3666),
    "Varadero, Cuba": (23.1547, -81.2546),
    "Cienfuegos, Cuba": (22.1613, -80.4490),
    "Belize City, Belize": (17.5046, -88.1962),
    "George Town, Cayman Islands": (19.2869, -81.3674),
    "Nassau, Bahamas": (25.0343, -77.3963)
}

currentPoint = target_cities["New Orleans, USA"]

#Retrieve dict of analogs
analogs = basin.analogs_from_point(currentPoint,radius=100,date_range=('5/1','10/1'),thresh={'v_min':65})

#Sort by ascending value, meaning the first entry is the smallest distance from the point
analogs_sorted = sorted(analogs.items(), key=lambda item: item[1])

#Get ID of closest storm, which will be the first item of the first entry of analogs_sorted
closest_storm = analogs_sorted[0][0]

#Plot storm
basin.plot_storm(closest_storm)