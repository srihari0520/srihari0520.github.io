#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <sstream>
#include <climits>

class myTree {
public:
    std::vector<myTree*> children;
    std::set<int> values;

    // Constructor
    myTree(std::set<int> val) : values(val) {}
};

// Function declarations
myTree* constructTree(const std::set<int>& nodeVals, const std::vector<std::vector<int>>& data, int dim, int maxDim);
void processPointQuery(myTree* root, const std::vector<int>& query, int dim, int maxDim, std::vector<int> path);
void processRangeQuery(myTree* root, const std::vector<std::pair<int, int>>& query, int dim, int maxDim, std::vector<int> path);
void printData(const std::vector<std::vector<int>>& data);
void printQueryOutput(std::vector<int> node);

int main() {
    int numDimensions, numRows, numQueries;
    std::cin >> numDimensions; // reading the number of dimensions from input
    std::cin >> numRows; // reading the number of rows from input
    
    // Creating vectors to store the data.
    std::vector<int> row;
    std::vector<std::vector<int>> data;

    // Reading file
    for (int i = 0; i < numRows; i++) {
        for (int j = 0; j < numDimensions; j++) {
            int temp;
            std::cin >> temp;
            row.push_back(temp);
        }
        data.push_back(row);
        row.clear();
    }

    // Construct the multidimensional attribute tree
    std::set<int> nodeVals;// Creating a set to store unique values of the first dimension
    for (size_t i = 0; i < data.size(); i++) {
        nodeVals.insert(data[i][0]);// Inserting the  values of first dimension into the set nodeVals
    }

    int dim = 0; //Choosing the dimension 0 as root
    int maxDim = numDimensions;// maximum number of dimensions

    myTree* root = constructTree(nodeVals, data, dim, maxDim);// Constructing the tree using the chosen dimension as the root

    // Processing the queries
    std::cin >> numQueries;

    // Storing point and range queries for further processing
    std::vector<std::pair<char, std::vector<int>>> pointQueries;// Vector to store point queries
    std::vector<std::pair<char, std::vector<std::pair<int, int>>>> rangeQueries;// Vector to store range queries

    for (int i = 0; i < numQueries; i++) {
        char queryType;
        std::cin >> queryType;// Input the type of query which is P for pointQueries and R for rangeQueries

        if (queryType == 'P') { // checking if the query type is a Point Query P
            std::vector<int> query(numDimensions);// Creating a vector to store the values of the point query and then initializing it with the numDimensions size
            for (int j = 0; j < numDimensions; j++) {
                std::cin >> query[j];// Taking the input of each dimension value for the point query
            }
            pointQueries.push_back({queryType, query});// Storing the point query in the vector point queries
        } else if (queryType == 'R') { // Range Query
            std::vector<std::pair<int, int>> query(numDimensions);
            for (int j = 0; j < numDimensions; j++) {
                std::cin >> query[j].first >> query[j].second; // taking  the range values from input for each dimension in the range query
            }
            rangeQueries.push_back({queryType, query});// Soring the rangeQuery in the rangeQueries vector
        }
    }

    // Processing Point Queries
    std::cout << "Outputting Point Query results:" << std::endl;// printing a message showing the beginning of point query results output.
    for (auto& pointQuery : pointQueries) {
        std::cout << "Point Query:";// printing out a sub heading which indicates the starting of a point query.
        for (int val : pointQuery.second) {
            std::cout << " " << val;// printing each value of the current point query.
        }
        std::cout << std::endl;// Moving to the next line after printing all values of the point query.
        processPointQuery(root, pointQuery.second, dim, maxDim, std::vector<int>());// Calling the processPointQuery function to handle the point query
    }

    // Processing Range Queries
    std::cout << "Outputting Range Query results:" << std::endl;// printing a heading which shows starting range query results output.
    for (auto& rangeQuery : rangeQueries) {
        std::cout << "Ranged Query:";// printing out a subheading which helps in showing the starting of range query.
        for (auto& range : rangeQuery.second) {
            std::cout << " " << range.first << ":" << range.second;// printing the first and last values of each dimension in the range query
        }
        std::cout << std::endl;// Moving to next line after printing all values of the range query
        processRangeQuery(root, rangeQuery.second, dim, maxDim, std::vector<int>());// Calling the processRangeQuery function 
    }

    // Print input file data
    printData(data);

    // Optional: Clean up tree memory (implement a function to delete the tree nodes)
    // ...
    delete root;

    return 0;
}

myTree* constructTree(const std::set<int>& nodeVals, const std::vector<std::vector<int>>& data, int dim, int maxDim) {
    myTree* root = new myTree({});// Creating a new tree with an empty set of values for the root.

    for (size_t i = 0; i < data.size(); i++) {// Iterating through the data to construct the tree.
        myTree* currentNode = root;// Starting from the root for each data point.

        for (int val : data[i]) {  // Iterating through each value in the data point.
            myTree* child = 0;// Initializing a pointer of the my tree node as 0

            
            int index = 0;//Initializing the index to zero
            for (auto &value : currentNode->values) {
                if (val == value) {// Checking if the value already exists in  the children
                    child = currentNode->children[index];// If the value exists, updating the child node 
                    break;
                }

                index += 1;// Incrementing the index 
            }

            
            if (child == nullptr) {// If the value doesn't exist n children,
                child = new myTree({}); //  creating a new node.

                currentNode->values.insert(val);// Inserting the value into the current node's values.
                currentNode->children.push_back(child);// storing the new node as a child.
            }

            currentNode = child;// Moving the currentNode to the child node
        }
    }

    return root;// returning the root of the constructed tree
}

void processPointQuery(myTree* root, const std::vector<int>& query, int dim, int maxDim, std::vector<int> path) {
    if (maxDim == dim) {// if the maximum dimension is reached which is when maxDim is equal to dimension dim
        printQueryOutput(path);// Printing the current path and return
        return;
    }

    int index = 0;// Initializing index to 0
    for (auto &value : root->values) {// Iterating through each value in the current node
        if (query[dim] == -1 || query[dim] == value) { // Checking if the query allows any value in the current dimension or matches the current value.
            std::vector<int> newPath(path);// creating a new path
            newPath.push_back(value);//storing the value in the new path vector
            processPointQuery(root->children[index], query, dim + 1, maxDim, newPath);// calling the function processPointQuery 
        index += 1;// Moving to the next value in the current node by incrementing the index
        }
    }
}

void processRangeQuery(myTree* root, const std::vector<std::pair<int, int>>& query, int dim, int maxDim, std::vector<int> path) {
    if (maxDim == dim) {// if the maximum dimension is reached which is when maxDim is equal to dimension dim
        printQueryOutput(path);//printing the current path and return
        return;
    }

    int index = 0;// Initializing index as 0
    for (auto &value : root->values) {// Iterating through each value in the current node
        int low = INT_MIN;// Initializing low to the minimum integer value
        int high = INT_MAX;// Initializing high to the maximum integer value.

        if (query[dim].first != -1) {// checking if the range query specifies an lower bound
            low = query[dim].first;// Updating low
        }
        if (query[dim].second != -1) {// checking if the range query specifies an upper bound
            high = query[dim].second;// updating high
        }

        if (value >= low && value <= high) {// Checking if the current value is within range.
            std::vector<int> newPath(path);// If it is within the range, then creating a new path 
            newPath.push_back(value);// storing the current value in the newpath vector
            processRangeQuery(root->children[index], query, dim + 1, maxDim, newPath);// calling processRangeQuery for the next dimension
        }
        index += 1;// Moving to the next value in the current node by incrementing the index
    }
}


void printQueryOutput(std::vector<int> node) {
    for (auto &value : node) {// Iterating through each value in the node vector
        std::cout << value << " "; // Printing the values in a node for a query output
    }
    std::cout << std::endl;// Moving to the next line after printing the values.
}

void printData(const std::vector<std::vector<int>>& data) {
    // Printing input file data
    std::cout << "Printing file data:" << std::endl;// printing a heading indicating the start of file data printing.
    for (auto& row : data) {// Iterating through each row of the data.
        for (int val : row) {// Iterating through each value in the row
            std::cout << val << " ";// Printing the values
        }
        std::cout << std::endl;// Moving to the next line after printing all values in the row.
    }
}