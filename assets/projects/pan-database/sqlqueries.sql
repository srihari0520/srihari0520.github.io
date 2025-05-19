-- Drop Tables if they exist
-- Dropping tables in reverse order of dependencies to avoid foreign key conflicts
DROP TABLE IF EXISTS Credit_Card;
DROP TABLE IF EXISTS [Check];
DROP TABLE IF EXISTS Donor;
DROP TABLE IF EXISTS Expense;
DROP TABLE IF EXISTS Reports;
DROP TABLE IF EXISTS Employee;
DROP TABLE IF EXISTS Cares;
DROP TABLE IF EXISTS Serves;
DROP TABLE IF EXISTS Leader;
DROP TABLE IF EXISTS Volunteer;
DROP TABLE IF EXISTS Team;
DROP TABLE IF EXISTS Covered_By;
DROP TABLE IF EXISTS Insurance_Policy;
DROP TABLE IF EXISTS Needs;
DROP TABLE IF EXISTS Client;
DROP TABLE IF EXISTS Emergency_Contact;
DROP TABLE IF EXISTS Person;

-- Creating Person table 
CREATE TABLE Person (
    SSN CHAR(9) PRIMARY KEY,
    profession VARCHAR(50),
    gender CHAR(1),
    name VARCHAR(100),
    mailing_address VARCHAR(255),
    phone_number VARCHAR(15),
    email_id VARCHAR(100),
    on_mailing_list BIT  
);

-- Creating Donor table 
CREATE TABLE Donor (
    SSN CHAR(9) PRIMARY KEY,
    anonymous BIT,  
    FOREIGN KEY (SSN) REFERENCES Person(SSN)
);

-- Creating Emergency_Contact table 
CREATE TABLE Emergency_Contact (
    name VARCHAR(100),
    phone_number VARCHAR(15),
    relationship VARCHAR(50),
    SSN CHAR(9),
    FOREIGN KEY (SSN) REFERENCES Person(SSN)
);

-- Creating Client table 
CREATE TABLE Client (
    SSN CHAR(9) PRIMARY KEY,
    doctor_name VARCHAR(100),
    date_joined DATE,
    doctor_phone_number VARCHAR(15),
    FOREIGN KEY (SSN) REFERENCES Person(SSN)
);

-- Creating index on SSN in Client table
CREATE NONCLUSTERED INDEX idx_client_ssn ON Client(SSN);

-- Creating Needs table 
CREATE TABLE Needs (
    type VARCHAR(50),
    importance INT,
    client_SSN CHAR(9),
    FOREIGN KEY (client_SSN) REFERENCES Client(SSN),
    PRIMARY KEY (client_SSN, type)
);

-- Creating index on importance in Needs table
CREATE NONCLUSTERED INDEX idx_needs_importance ON Needs(importance);

-- Creating Insurance_Policy table 
CREATE TABLE Insurance_Policy (
    policy_id CHAR(10) PRIMARY KEY,
    provider_name VARCHAR(100),
    provider_address VARCHAR(255),
    type VARCHAR(50)
);

-- Creating Covered_By table 
CREATE TABLE Covered_By (
    client_SSN CHAR(9),
    policy_id CHAR(10),
    PRIMARY KEY (client_SSN, policy_id),
    FOREIGN KEY (client_SSN) REFERENCES Client(SSN),
    FOREIGN KEY (policy_id) REFERENCES Insurance_Policy(policy_id)
);

-- Creating index on policy_id in Covered_By table
CREATE NONCLUSTERED INDEX idx_covered_by_policy_id ON Covered_By(policy_id);

-- Creating Team table 
CREATE TABLE Team (
    Name VARCHAR(100) PRIMARY KEY,
    type VARCHAR(50),
    date DATE
);

-- Creating index on date in Team table
CREATE NONCLUSTERED INDEX idx_team_date ON Team(date);

-- Creating Volunteer table 
CREATE TABLE Volunteer (
    SSN CHAR(9) PRIMARY KEY,
    date_joined DATE,
    recent_training_date DATE,
    recent_training_location VARCHAR(100),
    FOREIGN KEY (SSN) REFERENCES Person(SSN)
);

-- Creating index on SSN in Volunteer table
CREATE NONCLUSTERED INDEX idx_volunteer_ssn ON Volunteer(SSN);

-- Creating Leader table 
CREATE TABLE Leader (
    volunteer_SSN CHAR(9),
    Name VARCHAR(100),
    PRIMARY KEY (volunteer_SSN, Name),
    FOREIGN KEY (volunteer_SSN) REFERENCES Volunteer(SSN),
    FOREIGN KEY (Name) REFERENCES Team(Name)
);

-- Creating Serves table with month as part of the primary key
CREATE TABLE Serves (
    Volunteer_SSN CHAR(9),
    Team_Name VARCHAR(100),
    active_inactive CHAR(1),
    hours_worked INT,
    month INT CHECK (month BETWEEN 1 AND 12),
    PRIMARY KEY (Volunteer_SSN, Team_Name, month),
    FOREIGN KEY (Volunteer_SSN) REFERENCES Volunteer(SSN),
    FOREIGN KEY (Team_Name) REFERENCES Team(Name)
);

-- Creating index on Volunteer_SSN in Serves table
CREATE NONCLUSTERED INDEX idx_serves_volunteer_ssn ON Serves(Volunteer_SSN);

-- Creating Cares table 
CREATE TABLE Cares (
    Name VARCHAR(100),
    client_SSN CHAR(9),
    PRIMARY KEY (Name, client_SSN),
    FOREIGN KEY (Name) REFERENCES Team(Name),
    FOREIGN KEY (client_SSN) REFERENCES Client(SSN)
);

-- Creating index on client_SSN in Cares table
CREATE NONCLUSTERED INDEX idx_cares_client_ssn ON Cares(client_SSN);

-- Creating Employee table 
CREATE TABLE Employee (
    SSN CHAR(9) PRIMARY KEY,
    marital_status VARCHAR(10),
    hire_date DATE,
    salary DECIMAL(10, 2),
    FOREIGN KEY (SSN) REFERENCES Person(SSN)
);

-- Creating index on SSN in Employee table
CREATE NONCLUSTERED INDEX idx_employee_ssn ON Employee(SSN);

-- Creating Reports table 
CREATE TABLE Reports (
    employee_SSN CHAR(9),
    team_name VARCHAR(100),
    date DATE,
    description TEXT,
    PRIMARY KEY (employee_SSN, team_name, date),
    FOREIGN KEY (employee_SSN) REFERENCES Employee(SSN),
    FOREIGN KEY (team_name) REFERENCES Team(Name)
);

-- Creating index on employee_SSN in Reports table
CREATE NONCLUSTERED INDEX idx_reports_employee_ssn ON Reports(employee_SSN);

-- Creating Expense table 
CREATE TABLE Expense (
    employee_SSN CHAR(9),
    date DATE,
    amount DECIMAL(10, 2),
    description TEXT,
    PRIMARY KEY (employee_SSN, date),
    FOREIGN KEY (employee_SSN) REFERENCES Employee(SSN)
);

-- Creating index on date in Expense table
CREATE NONCLUSTERED INDEX idx_expense_date ON Expense(date);

-- Creating Check table 

CREATE TABLE [Check] (
    Donor_SSN CHAR(9),
    date DATE,
    check_number CHAR(10),
    campaign_name VARCHAR(100),
    type VARCHAR(50),
    amount DECIMAL(10, 2),  
    PRIMARY KEY (Donor_SSN, check_number),
    FOREIGN KEY (Donor_SSN) REFERENCES Donor(SSN)
);

-- Creating index on Donor_SSN in Check table
CREATE NONCLUSTERED INDEX idx_check_donor_ssn ON [Check](Donor_SSN);

-- Creating Credit_Card table 
CREATE TABLE Credit_Card (
    Donor_SSN CHAR(9),
    date DATE,
    card_number CHAR(16),
    card_type VARCHAR(50),
    Amount DECIMAL(10, 2),
    type VARCHAR(50),
    campaign_name VARCHAR(100),
    expiration_date DATE,
    PRIMARY KEY (Donor_SSN, card_number),
    FOREIGN KEY (Donor_SSN) REFERENCES Donor(SSN)
);

-- Creating index on Donor_SSN in Credit_Card table
CREATE NONCLUSTERED INDEX idx_credit_card_donor_ssn ON Credit_Card(Donor_SSN);

SELECT*FROM Team

SELECT*FROM Person
SELECT*FROM Volunteer
SELECT*FROM Serves
SELECT*FROM Employee
SELECT*FROM Expense
SELECT*FROM Donor
SELECT*FROM [Check]
SELECT*FROM Credit_Card
SELECT*FROM Needs
SELECT*FROM Insurance_Policy
SELECT*FROM Client












