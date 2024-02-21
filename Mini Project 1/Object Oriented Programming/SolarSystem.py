import json
import csv

planet1 = {
    'Name': 'Mercury', 
    'OrbitalPeriod': 0.39,  
    'Circumference': 15329, 
    }

planet2 = {
    'Name': 'Venus', 
    'DistanceFromSun': 0.72,  
    'Diameter': 12104, 
    }

moon1 = {'Name': 'Luna', 'Diameter': 3474, 'Circumference': 10917}
planet3 = {
    'Name': 'Earth', 
    'DistanceFromSun': 1, 
    'OrbitalPeriod': 1, 
    'Diameter': 12756, 
    'Circumference': 40075, 
    'Moons': [moon1]
    }

moon1 = {'Name': 'Phobos', 'Diameter': 22.5}
moon2 = {'Name': 'Deimos', 'Circumference': 39}
planet4 = {
    'Name': 'Mars', 
    'DistanceFromSun': 1.52, 
    'Circumference': 21344, 
    'Moons': [moon1, moon2]
    }

moon1 = {'Name': 'Ganymede', 'Diameter': 5268}
moon2 = {'Name': 'Callisto', 'Circumference': 4820.6}
moon3 = {'Name': 'Io', 'Circumference': 3643.2}
planet5 = {
    'Name': 'Jupiter', 
    'DistanceFromSun': 5.2, 
    'Diameter': 142984, 
    'Moons': [moon1, moon2, moon3]
    }

planet6 = {
    'Name': 'Saturn', 
    'DistanceFromSun': 9.54, 
    'Diameter': 120536, 
    'Moons': []
    }

planet7 = {
    'Name': 'Uranus', 
    'DistanceFromSun': 19.2, 
    'Diameter': 51118, 
    }

planet8 = {
    'Name': 'Neptune', 
    'DistanceFromSun': 30.06, 
    'Diameter': 49528, 
    }

sun = {
    'Name': 'Sol',  
    'Diameter': 1400000, 
    'Planets': [planet1, planet2, planet3, planet4, planet5, planet6, planet7, planet8]
    }

ssJson = json.dumps(sun)
print(ssJson)

