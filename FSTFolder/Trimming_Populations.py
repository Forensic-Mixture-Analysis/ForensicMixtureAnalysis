#necessary imports.
import random as rm
import numpy as np
import csv

#empty lists that will be filled.
FSTarray_list = []
pop_names_list = []
deleted_pops_index = []
deleted_pops = []
pops_in_matrix = []

#import FSTarray data. Skip first line because it is the pop names. Append the
#data into the FSTarray_list. Then, convert it into an numpy array.
with open('FSTarray_no_FBI.csv', 'r') as FSTarray:
    FSTarray_reader = csv.reader(FSTarray)

    next(FSTarray_reader)

    for line in FSTarray_reader:
        FSTarray_list.append(list(map(float, line)))

#import pop names data. Skip first line because it is the column names. Append the
#data into pop_names_list.
FSTarray_matrix = np.array(FSTarray_list)

with open('pop_names_no_FBI.csv', 'r') as pop_names:
    pop_names_reader = csv.reader(pop_names)

    next(pop_names_reader)

    for pops in pop_names_reader:
        pop_names_list.append(pops[1])

#Removing the values that fall on the diagonal of the matrix.
for i in range(len(FSTarray_matrix)):
    FSTarray_matrix[i][i] = 100

#This block of code prepares for a dictionary where the keys are the indices of the FST_array_matrix,
#and the values are the values of the matrix itself. In order to do this, first we need to
#determine the number of rows and columns, as well as create a list of range for rows and columns.
num_of_rows = len(FSTarray_matrix)
range_of_rows = list(range(num_of_rows))
num_of_cols = len(FSTarray_matrix[0])
range_of_cols = list(range(num_of_cols))

#Next, make a list of all the values from the FST_array_matrix.
all_FSTarray_values = []
for rows in FSTarray_matrix:
    for matrix_value in rows:
        all_FSTarray_values.append(matrix_value)

#Next, create a dictionary with matrix index tuples for keys with no values. Take
#the keys and generate a second list.
FSTarray_dict = {}
for row_numbers in range_of_rows:
    for col_numbers in range_of_cols:
        FSTarray_dict[row_numbers, col_numbers] = ()
indices = FSTarray_dict.keys()

#Lastly, zip together the two lists into a dictionary.
FSTarray_dict = dict(zip(indices, all_FSTarray_values))

#find the lowest value of a matrix.
lowest_value = FSTarray_matrix.min()
#print('Lowest value in above matrix:', lowest_value)

#choose minimum value for all values in matrix to exceed.
while lowest_value < 0.005:

#find the indices of lowest value.
    location = np.where(FSTarray_matrix == lowest_value)
    axis_1, axis_2 = list(location[0]), list(location[1])

#this block of code is relevant only if there are duplicates of the lowest value.
#It serves to 1) find the lowest value(s) in matrix, 2) determine if
#there are more than one lowest value. If so, 3) it randomly selects one of the values
#to continue with deleting rows/columns.
    if len(axis_1) > 1:
        #print("There are duplicates of the lowest value. Randomly choosing one...")
        multiple_lowest = list(range(len(axis_1)))
        chosen_lowest = rm.sample(multiple_lowest, 1)
        strings = [str(int) for int in chosen_lowest]
        a_string = "".join(strings)
        b_int = int(a_string)
        for t in axis_1:
            del axis_1[not b_int]
        for t in axis_2:
            del axis_2[not b_int]

#randomly selects row or column of lowest value, deletes it, then deletes the
#other axis of the same number axis. For example, if row 3 is chosen, it will
#delete row 3, then delete column 3 of the matrix.
    rando = rm.sample([axis_1, axis_2], 2)
    if rando[0] == axis_1:
        #print("Index of lowest value:", rando[0], rando[1])
        FSTarray_matrix[axis_1] = 100
        FSTarray_matrix[:, axis_1] = 100
        deleted_pops_index += axis_1
        #print('Randomly chosen axis: row. Deleting row and column {}'.format(rando[0]))
        #print(a)
    elif rando[0] == axis_2:
        #print("Index of lowest value:", rando[1], rando[0])
        FSTarray_matrix[axis_2]= 100
        FSTarray_matrix[:, axis_2] = 100
        deleted_pops_index += axis_2
        #print('Randomly chosen axis: column. Deleting row and column {}'.format(rando[0]))
        #print(a)
    lowest_value = FSTarray_matrix.min()

#create a list of the new values of the matrix. 100 represents that the
#value was deleted from the matrix.
final_FSTarray_values = [values for rows in FSTarray_matrix for values in rows]

#for any deleted values from the matrix, it is made into a tuple that has the
#deleted value, and the string 'deleted' next to it. Values that are still in the
#matrix are unmodified. Everything is then zipped into a final list.
final_values_status = []
for (original, final) in zip(all_FSTarray_values, final_FSTarray_values):
    if final == 100:
        final = (original, "deleted")
        final_values_status.append(final)
    else:
        final_values_status.append(final)

#zip together the list of keys of indices from original matrix, and the last list
#of values (which also marks deleted values).
final_FSTarray_matrix = dict(zip(indices, final_values_status))
#print("final_FSTarray_matrix:", final_FSTarray_matrix)
deleted_pops_index.sort()

for number, pop in enumerate(pop_names_list):
    if number in deleted_pops_index:
        deleted_pops.append(pop)
    else:
        pops_in_matrix.append(pop)
print('deleted pops:', deleted_pops)
print('\npops in matrix:', pops_in_matrix)
print('\nnumber of pops in matrix:', len(pops_in_matrix))
