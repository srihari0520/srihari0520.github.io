-- QUERY 1
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_EnterNewTeam;

-- Creating a procedure to insert a new team
GO
CREATE PROCEDURE sp_EnterNewTeam(
    @TeamName VARCHAR(100),
    @TeamType VARCHAR(50),
    @Date DATE
)
AS
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Team WHERE Name = @TeamName)
    BEGIN
        INSERT INTO Team (Name, type, date) 
        VALUES (@TeamName, @TeamType, @Date);
        PRINT CONCAT(@TeamName, ' added successfully.');
    END
    ELSE
    BEGIN
        PRINT CONCAT(@TeamName, ' already exists. No new entry added.');
    END
END;
GO

-- QUERY 2
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_EnterNewClient;

GO
CREATE PROCEDURE sp_EnterNewClient(
    @ClientSSN CHAR(9),
    @Name VARCHAR(50),
    @Gender CHAR(1),
    @Profession VARCHAR(50),
    @MailingAddress VARCHAR(255),
    @PhoneNumber VARCHAR(15),
    @EmailID VARCHAR(100),
    @OnMailingList BIT,
    @DoctorName VARCHAR(50),
    @DateJoined DATE,
    @DoctorPhoneNumber VARCHAR(15),
    @TeamName VARCHAR(100)
)
AS
BEGIN
    
    IF NOT EXISTS (SELECT 1 FROM Person WHERE SSN = @ClientSSN)
    BEGIN
        INSERT INTO Person (SSN, name, gender, profession, mailing_address, phone_number, email_id, on_mailing_list)
        VALUES (@ClientSSN, @Name, @Gender, @Profession, @MailingAddress, @PhoneNumber, @EmailID, @OnMailingList);
        PRINT 'Person entry created successfully.';
    END

    
    IF NOT EXISTS (SELECT 1 FROM Client WHERE SSN = @ClientSSN)
    BEGIN
        INSERT INTO Client (SSN, doctor_name, date_joined, doctor_phone_number) 
        VALUES (@ClientSSN, @DoctorName, @DateJoined, @DoctorPhoneNumber);
        
        
        INSERT INTO Cares (Name, client_SSN) VALUES (@TeamName, @ClientSSN);
        PRINT 'Client added successfully and associated with team.';
    END
    ELSE
    BEGIN
        PRINT 'Client already exists. No new entry added.';
    END
END;
GO

-- QUERY 3
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_AddPersonAndVolunteer;

GO
CREATE PROCEDURE sp_AddPersonAndVolunteer(
    @SSN CHAR(9),
    @Name VARCHAR(50),
    @Gender CHAR(1),
    @Profession VARCHAR(50),
    @MailingAddress VARCHAR(100),
    @Email VARCHAR(50),
    @PhoneNumber VARCHAR(15),
    @OnMailingList BIT,
    @DateJoined DATE,
    @RecentTrainingDate DATE,
    @RecentTrainingLocation VARCHAR(100),
    @TeamName VARCHAR(100),
    @Status CHAR(1),
    @HoursWorked INT
)
AS
BEGIN
    
    BEGIN TRANSACTION;

    
    IF NOT EXISTS (SELECT 1 FROM Person WHERE SSN = @SSN)
    BEGIN
        INSERT INTO Person (SSN, name, gender, profession, mailing_address, email_id, phone_number, on_mailing_list)
        VALUES (@SSN, @Name, @Gender, @Profession, @MailingAddress, @Email, @PhoneNumber, @OnMailingList);
        PRINT 'Person entry created successfully.';
    END
    ELSE
    BEGIN
        PRINT 'Person with this SSN already exists. Proceeding to add volunteer details.';
    END

    
    INSERT INTO Volunteer (SSN, date_joined, recent_training_date, recent_training_location)
    VALUES (@SSN, @DateJoined, @RecentTrainingDate, @RecentTrainingLocation);

    
    INSERT INTO Serves (Volunteer_SSN, Team_Name, active_inactive, hours_worked)
    VALUES (@SSN, @TeamName, @Status, @HoursWorked);

   
    COMMIT TRANSACTION;

    PRINT 'Volunteer and person entries added successfully.';
END;
GO


GO
DROP PROCEDURE IF EXISTS sp_EnterVolunteerHours;

GO
CREATE PROCEDURE sp_EnterVolunteerHours(
    @VolunteerSSN CHAR(9),
    @TeamName VARCHAR(100),
    @AdditionalHours INT,
    @Month INT,
    @Status CHAR(1)  
)
AS
BEGIN
    
    IF @Month BETWEEN 1 AND 12
    BEGIN
        
        IF EXISTS (SELECT 1 FROM Serves WHERE Volunteer_SSN = @VolunteerSSN AND Team_Name = @TeamName AND month = @Month)
        BEGIN
            
            UPDATE Serves 
            SET hours_worked = hours_worked + @AdditionalHours
            WHERE Volunteer_SSN = @VolunteerSSN AND Team_Name = @TeamName AND month = @Month;
            PRINT 'Volunteer hours updated successfully for the month.';
        END
        ELSE
        BEGIN
            
            INSERT INTO Serves (Volunteer_SSN, Team_Name, hours_worked, month, active_inactive)
            VALUES (@VolunteerSSN, @TeamName, @AdditionalHours, @Month, @Status);
            PRINT 'New month entry added with volunteer hours.';
        END
    END
    ELSE
    BEGIN
        
        PRINT 'Error: Month must be between 1 and 12.';
    END
END;
GO



-- QUERY 5
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_EnterNewEmployee;

-- Creating a procedure to insert a new employee and associate with teams
GO
CREATE PROCEDURE sp_EnterNewEmployee(
    @EmployeeSSN CHAR(9),
    @Name VARCHAR(50),
    @Gender CHAR(1),
    @Profession VARCHAR(50),
    @MailingAddress VARCHAR(100),
    @Email VARCHAR(50),
    @PhoneNumber VARCHAR(15),
    @OnMailingList BIT,
    @MaritalStatus VARCHAR(20),
    @HireDate DATE,
    @Salary DECIMAL(10, 2),
    @TeamName VARCHAR(100),
    @ReportDate DATE,
    @ReportDescription VARCHAR(200)
)
AS
BEGIN
    
    BEGIN TRANSACTION;

   
    IF NOT EXISTS (SELECT 1 FROM Person WHERE SSN = @EmployeeSSN)
    BEGIN
        INSERT INTO Person (SSN, name, gender, profession, mailing_address, email_id, phone_number, on_mailing_list)
        VALUES (@EmployeeSSN, @Name, @Gender, @Profession, @MailingAddress, @Email, @PhoneNumber, @OnMailingList);
        PRINT 'Person entry created successfully.';
    END
    ELSE
    BEGIN
        PRINT 'Person with this SSN already exists. Proceeding to add employee details.';
    END

    
    INSERT INTO Employee (SSN, marital_status, hire_date, salary) 
    VALUES (@EmployeeSSN, @MaritalStatus, @HireDate, @Salary);

    
    INSERT INTO Reports (employee_SSN, team_name, date, description) 
    VALUES (@EmployeeSSN, @TeamName, @ReportDate, @ReportDescription);

    
    COMMIT TRANSACTION;

    PRINT 'Employee and person entries added successfully and associated with team.';
END;
GO

-- QUERY 6
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_EnterExpense;

-- Creating a procedure to enter an expense charged by an employee
GO
CREATE PROCEDURE sp_EnterExpense(
    @EmployeeSSN CHAR(9),
    @ExpenseDate DATE,
    @Amount DECIMAL(10, 2),
    @Description VARCHAR(200)
)
AS
BEGIN
    IF EXISTS (SELECT 1 FROM Employee WHERE SSN = @EmployeeSSN)
    BEGIN
        INSERT INTO Expense (employee_SSN, date, amount, description) 
        VALUES (@EmployeeSSN, @ExpenseDate, @Amount, @Description);
        PRINT 'Expense entry created successfully.';
    END
    ELSE
    BEGIN
        PRINT 'Error: Employee SSN does not exist. Cannot create expense entry.';
    END
END;
GO


GO
DROP PROCEDURE IF EXISTS sp_EnterNewDonor;

GO
CREATE OR ALTER PROCEDURE sp_EnterNewDonor(
    @SSN CHAR(9),
    @Name VARCHAR(100),
    @Gender CHAR(1),
    @Profession VARCHAR(50),
    @MailingAddress VARCHAR(255),
    @PhoneNumber VARCHAR(15),
    @EmailID VARCHAR(100),
    @OnMailingList BIT,
    @Anonymous BIT,
    @DonationCount INT,
    @DonationType NVARCHAR(20), 
    @Amount DECIMAL(10, 2),
    @Date DATE,
    @ChequeNo CHAR(10) = NULL,
    @CardNumber CHAR(16) = NULL,
    @CardType VARCHAR(50) = NULL,
    @ExpiryDate DATE = NULL,
    @CampaignName VARCHAR(100)
)
AS
BEGIN
    
    SET @DonationType = LTRIM(RTRIM(LOWER(@DonationType))); 
    PRINT 'Donation Type after conversion: ' + @DonationType; 

    
    IF NOT EXISTS (SELECT * FROM Person WHERE SSN = @SSN)
    BEGIN
        INSERT INTO Person (SSN, profession, gender, name, mailing_address, phone_number, email_id, on_mailing_list)
        VALUES (@SSN, @Profession, @Gender, @Name, @MailingAddress, @PhoneNumber, @EmailID, @OnMailingList);
    END

    
    IF NOT EXISTS (SELECT * FROM Donor WHERE SSN = @SSN)
    BEGIN
        INSERT INTO Donor (SSN, anonymous)
        VALUES (@SSN, @Anonymous);
    END

    
    DECLARE @i INT = 1;
    WHILE @i <= @DonationCount
    BEGIN
        IF @DonationType = 'cheque'
        BEGIN
            PRINT 'Inserting into Check table'; -- Debugging line
            INSERT INTO [Check] (Donor_SSN, date, check_number, campaign_name, type, amount)
            VALUES (@SSN, @Date, @ChequeNo, @CampaignName, 'Cheque', @Amount);
        END
        ELSE IF @DonationType = 'credit card'
        BEGIN
            PRINT 'Inserting into Credit_Card table'; -- Debugging line
            INSERT INTO Credit_Card (Donor_SSN, date, card_number, card_type, amount, type, campaign_name, expiration_date)
            VALUES (@SSN, @Date, @CardNumber, @CardType, @Amount, 'Credit Card', @CampaignName, @ExpiryDate);
        END
        ELSE
        BEGIN
            RAISERROR ('Invalid Donation Type. Must be either "Cheque" or "Credit Card".', 16, 1);
            RETURN;
        END
        SET @i = @i + 1;
    END
END;
GO

-- QUERY 8
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_RetrieveClientDoctor;

-- Creating a procedure to retrieve the doctor information of a client
GO
CREATE PROCEDURE sp_RetrieveClientDoctor(
    @ClientSSN CHAR(9)
)
AS
BEGIN
    SELECT doctor_name, doctor_phone_number 
    FROM Client 
    WHERE SSN = @ClientSSN;
END;
GO

-- QUERY 9
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_TotalExpensesByEmployee;

-- Creating a procedure to retrieve total expenses charged by each employee for a period
GO
CREATE PROCEDURE sp_TotalExpensesByEmployee(
    @StartDate DATE,
    @EndDate DATE
)
AS
BEGIN
    SELECT employee_SSN, SUM(amount) AS total_expenses 
    FROM Expense 
    WHERE date BETWEEN @StartDate AND @EndDate 
    GROUP BY employee_SSN 
    ORDER BY total_expenses DESC;
END;
GO

-- QUERY 10
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_VolunteersForClientTeams;

-- Creating a procedure to list volunteers for a client's teams
GO
CREATE PROCEDURE sp_VolunteersForClientTeams(
    @ClientSSN CHAR(9)
)
AS
BEGIN
    SELECT Volunteer.SSN, Volunteer.recent_training_location 
    FROM Volunteer 
    JOIN Serves ON Volunteer.SSN = Serves.Volunteer_SSN 
    JOIN Cares ON Serves.Team_Name = Cares.Name 
    WHERE Cares.client_SSN = @ClientSSN;
END;
GO



-- QUERY 11
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_RetrieveTeamsFoundedAfterDate;

-- Creating a procedure to retrieve team names founded after a specified date
GO
CREATE PROCEDURE sp_RetrieveTeamsFoundedAfterDate(
    @Date DATE
)
AS
BEGIN
    SELECT Name
    FROM Team
    WHERE date > @Date;
END;
GO

-- QUERY 12
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_RetrieveAllPeopleInfo;

-- Creating a procedure to retrieve all personal and emergency contact information for everyone in the database
GO
CREATE PROCEDURE sp_RetrieveAllPeopleInfo
AS
BEGIN
    SELECT 
        P.SSN, 
        P.name AS PersonName, 
        P.phone_number AS PhoneNumber, 
        P.email_id AS EmailID, 
        P.mailing_address AS MailingAddress,
        EC.name AS EmergencyContactName, 
        EC.phone_number AS EmergencyContactPhone, 
        EC.relationship AS EmergencyContactRelation
    FROM 
        Person P
    LEFT JOIN 
        Emergency_Contact EC ON P.SSN = EC.SSN;
END;
GO

-- QUERY 13
-- Dropping the procedure if it already exists

GO
DROP PROCEDURE IF EXISTS sp_RetrieveEmployeeDonors;

GO
CREATE PROCEDURE sp_RetrieveEmployeeDonors
AS
BEGIN
    SELECT 
        D.SSN, 
        D.anonymous AS IsAnonymous, 
        ISNULL(SUM(CC.amount), 0) AS TotalCreditDonation,
        ISNULL(SUM(CK.amount), 0) AS TotalCheckDonation,
        ISNULL(SUM(CC.amount), 0) + ISNULL(SUM(CK.amount), 0) AS TotalDonation
    FROM 
        Donor D
    LEFT JOIN 
        Credit_Card CC ON D.SSN = CC.Donor_SSN
    LEFT JOIN 
        [Check] CK ON D.SSN = CK.Donor_SSN
    GROUP BY 
        D.SSN, D.anonymous
    HAVING 
        ISNULL(SUM(CC.amount), 0) + ISNULL(SUM(CK.amount), 0) > 0 
    ORDER BY 
        TotalDonation DESC;
END;
GO






-- QUERY 14
-- Dropping the procedure if it already exists
GO
DROP PROCEDURE IF EXISTS sp_IncreaseSalaryForMultiTeamEmployees;

-- Creating a procedure to increase salary by 10% for employees managing multiple teams
GO
CREATE PROCEDURE sp_IncreaseSalaryForMultiTeamEmployees
AS
BEGIN
    UPDATE Employee
    SET salary = salary * 1.10
    WHERE SSN IN (
        SELECT employee_SSN
        FROM Reports
        GROUP BY employee_SSN
        HAVING COUNT(DISTINCT team_name) > 1
    );
    PRINT 'Salaries updated successfully for eligible employees.';
END;
GO

--Query 15
GO
DROP PROCEDURE IF EXISTS sp_DeleteUninsuredClientsWithLowTransportValue;

GO
CREATE PROCEDURE sp_DeleteUninsuredClientsWithLowTransportValue
AS
BEGIN
    
    DELETE FROM Cares
    WHERE client_SSN IN (
        SELECT C.SSN
        FROM Client C
        LEFT JOIN Covered_By CB ON C.SSN = CB.client_SSN
        LEFT JOIN Needs N ON C.SSN = N.client_SSN AND N.type = 'Transportation'
        WHERE CB.policy_id IS NULL           
          AND (N.importance IS NULL OR N.importance < 5)
    );

   
    DELETE FROM Needs
    WHERE client_SSN IN (
        SELECT C.SSN
        FROM Client C
        LEFT JOIN Covered_By CB ON C.SSN = CB.client_SSN
        LEFT JOIN Needs N ON C.SSN = N.client_SSN AND N.type = 'Transportation'
        WHERE CB.policy_id IS NULL            
          AND (N.importance IS NULL OR N.importance < 5)
    );

    
    DELETE FROM Client
    WHERE SSN IN (
        SELECT C.SSN 
        FROM Client C
        LEFT JOIN Covered_By CB ON C.SSN = CB.client_SSN
        LEFT JOIN Needs N ON C.SSN = N.client_SSN AND N.type = 'Transportation'
        WHERE CB.policy_id IS NULL           
          AND (N.importance IS NULL OR N.importance < 5) 
    );

    PRINT 'Eligible clients and their associated records in Needs and Cares were deleted successfully.';
END;
GO




-- QUERY 17
-- Drop the procedure if it exists

DROP PROCEDURE IF EXISTS sp_RetrieveMailingList;
GO

-- Creating a new stored procedure to retrieve mailing list data
CREATE PROCEDURE sp_RetrieveMailingList
AS
BEGIN
    SELECT name, mailing_address 
    FROM Person 
    WHERE on_mailing_list = 1;
END;
GO


