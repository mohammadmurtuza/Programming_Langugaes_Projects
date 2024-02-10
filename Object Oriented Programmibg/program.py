from celestialbody import *
from SolarSystem import *

class SolarSystem:
    def __init__(self, data):
        self.sun = Sun(data['Name'], data['Diameter'])
        self.planets = [self.planet(**planet) for planet in data.get('Planets', [])]

    def planet(self, **kwargs):
        planet_kwargs = {
            'name': kwargs.get('Name'),
            'diameter': kwargs.get('Diameter'),
            'circumference': kwargs.get('Circumference'),
            'distance_from_sun': kwargs.get('DistanceFromSun'),
            'orbital_period': kwargs.get('OrbitalPeriod'),
            'moons': [self.moon(**moon) for moon in kwargs.get('Moons', [])]
        }

        planet_kwargs = {k: v for k, v in planet_kwargs.items() if v is not None}
        planet = Planet(**planet_kwargs)
        if planet.diameter is None:
            planet.diameter = planet.C_to_D()
        elif planet.circumference is None:
            planet.circumference = planet.D_to_C()        
        if planet.distance_from_sun is None:
            planet.distance_from_sun = planet.Orbit_to_Distance()
        elif planet.orbital_period is None:
            planet.orbital_period = planet.Distance_to_Orbit()        
        return planet
    
    def moon(self, **kwargs):
        moon_kwargs = {
            'name': kwargs.get('Name'),
            'diameter': kwargs.get('Diameter'),
            'circumference': kwargs.get('Circumference')
        }
        moon_kwargs = {k: v for k, v in moon_kwargs.items() if v is not None}
        moon = Moon(**moon_kwargs)
        if moon.diameter is None:
            moon.diameter = moon.C_to_D()
        elif moon.circumference is None:
            moon.circumference = moon.D_to_C()        
        return moon