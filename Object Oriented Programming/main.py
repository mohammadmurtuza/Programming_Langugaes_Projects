from SolarSystem import sun as details
from program import *
from art import logo

# Main function
def main():
    print(logo)
    solar_system = SolarSystem(details)
    print(f"Sun: {solar_system.sun.name}")
    print(f"Diameter: {solar_system.sun.diameter:,} km")
    print(f"Circumference: {solar_system.sun.circumference:,.0f} km")
    print()
    S_volume = solar_system.sun.volume
    T_volume = 0  
    
    print("_____________________________________________________________________________")
    
    for planet in solar_system.planets:
        print()
        print(f"Planet: {planet.name}")
        print(f"Distance from sun: {planet.distance_from_sun} AU")
        print(f"Orbital period: {planet.orbital_period} years")
        print(f"Diameter: {planet.diameter:,} km" )
        print(f"Circumference: {planet.circumference:,.0f} km")
        P_volume = planet.volume  
        T_volume += P_volume
        if planet.moons:
            print("Moons:")
            for moon in planet.moons:
                print(f"->{moon.name}")
                print(f"  ->Diameter: {moon.diameter:,} km")
                print(f"  ->Circumference: {moon.circumference:,.0f} km")
                 
        print()
        print("_____________________________________________________________________________")
    print()
    print(f"Volume of the Sun: {S_volume:,.0f} cubic km")
    print(f"Total Volume of all the Planets: {T_volume:,.0f} cubic km")
    
    planets_can_fit_in_sun = T_volume > S_volume
    print(f"All the planet's volumes added together could fit in the Sun: {planets_can_fit_in_sun}")
    print("_____________________________________________________________________________")

if __name__ == "__main__":
    main()