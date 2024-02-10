from SolarSystem import *
import math


def D_to_C(diameter):
    diameter = diameter * math.pi
    return diameter


def C_to_D(circumference):
    circumference = circumference / math.pi
    return circumference


def Distance_and_Orbit(planets):
    a =[]
    for planet in planets:
        P_distance_from_sun = planet.get('DistanceFromSun')
        P_orbital_period = planet.get('OrbitalPeriod')

        if P_distance_from_sun is None:
             # Calculate DistanceFromSun from OrbitalPeriod
            P_distance_from_sun = (P_orbital_period ** 2) ** (1. / 3.)
        elif P_orbital_period is None:
            # Calculate OrbitalPeriod from DistanceFromSun
            P_orbital_period = (P_distance_from_sun ** 3) ** 0.5

        a.append({
            'Name': planet.get('Name', 'Unknown'),
            'DistanceFromSun': P_distance_from_sun,
            'OrbitalPeriod': P_orbital_period
        })

    return a


def Planet_Diameter_and_Circumference(planets):
    a=[]

    for planet in planets:
        P_diameter = planet.get('Diameter')
        P_circumference = planet.get('Circumference')

        if P_diameter and not P_circumference:
            # Calculate Circumference from Diameter
            P_circumference = D_to_C(P_diameter)
        elif P_circumference and not P_diameter:
            # Calculate Diameter from Circumference
            P_diameter = C_to_D(P_circumference)

        a.append({
        'Name': planet.get('Name', 'Unknown'),
        'Diameter': P_diameter,
        'Circumference': P_circumference
    })
    
    return a


def Moon_D_and_C(moons):
    a =[]

    for moon in moons:
        M_diameter = moon.get('Diameter')
        M_circumference = moon.get('Circumference')

        if M_diameter and not M_circumference:
            # Calculate Circumference from Diameter
            M_circumference = D_to_C(M_diameter)
        elif M_circumference and not M_diameter:
            # Calculate Diameter from Circumference
            M_diameter = C_to_D(M_circumference)
        
        a.append({
            'Name': moon.get('Name', 'Unknown'),
            'Diameter': M_diameter,
            'Circumference': M_circumference
        })
    
    return a


def volume(diameter):
    radius = diameter / 2
    volume = (4/3) * math.pi * (radius ** 3)
    return volume


def Sun_details(): 
    S_name = sun.get('Name')
    S_diameter = sun.get('Diameter')
    S_circumference = sun.get('Circumference')

    if S_circumference is None:
        S_circumference = D_to_C(S_diameter)
    elif S_diameter is None:
        S_diameter = C_to_D(S_circumference)

    print(f"Sun: {S_name}")
    print(f"Diameter: {S_diameter:,} km")
    print(f"Circumference: {S_circumference:,} km")
    print()

    
def Planet_details():
    planets = sun.get('Planets',[])
    P_distance_and_orbital_period = Distance_and_Orbit(planets)
    P_Diameter_and_Circumference = Planet_Diameter_and_Circumference(planets)

    for i, j in enumerate(P_distance_and_orbital_period):
        P_name = j['Name']
        P_distance_from_sun = j['DistanceFromSun']
        P_orbital_period = j['OrbitalPeriod']
        P_diameter = P_Diameter_and_Circumference[i]['Diameter']
        P_circumference = P_Diameter_and_Circumference[i]['Circumference']

        moons = planets[i].get('Moons', [])  
        print("_____________________________________________________________________________")
        print()
        print(f"Planet: {P_name}")
        print(f"Distance from sun: {P_distance_from_sun:.2f} au")
        print(f"Orbital period: {P_orbital_period:.2f} yr")
        print(f"Diameter: {P_diameter:,} km")
        print(f"Circumference: {P_circumference:,} km")
        

        # Check if the planet has moons
        if moons:
            print("Moons:")
            moon_data = Moon_D_and_C(moons)
            for moon in moon_data:
                M_name = moon['Name']
                M_diameter = moon['Diameter']
                M_circumference = moon['Circumference']
                print(f"-> {M_name}:")
                print(f"  -> Diameter: {M_diameter:,} km")
                print(f"  -> Circumference: {M_circumference:,} km")
        
        print()

        
def Check_volume():

    S_diameter = sun.get('Diameter')
    S_volume = volume(S_diameter)
    planets = sun.get('Planets', [])
    P_total_volume = sum([volume(planet.get('Diameter', 0)) for planet in planets])

    planets_can_fit_in_sun = P_total_volume > S_volume
    print("_____________________________________________________________________________")
    print()
    print('Volume of the Sun: ', S_volume)
    print('Total Volume of all the Planets:', P_total_volume)
    print("All the planet's volumes added together could fit in the Sun:", planets_can_fit_in_sun)
    print("_____________________________________________________________________________")

    


