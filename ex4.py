# assign 100 to variable "cars", which is the number of cars.
cars = 100
# assign 4.0 to variable "space_in_a_car", which is the maximal number of passengers one car can have.
space_in_a_car = 4.0
# assign 30 to variable "drivers", which is the number of drives
drivers = 30
# assign 90 to variable "passengers", which is the number of total passengers.
passengers = 90
# create a variable "cars_not_driven", which is number of cars minus number of drivers.
cars_not_driven = cars - drivers
# create a variable "cars_driven", which equals the number of drivers.
cars_driven = drivers
# create a variable "carpool_capacity", which is number of cars driven multiply the maximal amount of passengers one car can have.
carpool_capacity = cars_driven * space_in_a_car
# create a variable "average_passengers_per_car", which is the number of passengers divided by the number of cars driven.
average_passengers_per_car = passengers / cars_driven


print("There are", cars, "cars available.")
print("There are only", drivers, "drivers available.")
print("There will be", cars_not_driven, "empty cars today.")
print("We can transport", carpool_capacity, "people today.")
print("We have", passengers, "to carpool today.")
print("We need to put about", average_passengers_per_car, "in each car.")

# Python returned that error because in the 8th line, the writer typed a wrong variable name "car_pool_capacity" instead of the right one "carpool_capacity", while the "car_pool_capacity" is not defined in the above line, so python cannot work on this variable.

# Using 4.0 is not necessary, but it is more precise than using 4. If just use 4, then the value of carpool_capacity, which is space_in_a_car multiplying cars_driven, will be 120 - an integer, instead of 120.0 - a floating point number.
