import java.sql.*;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Scanner;
import java.math.BigDecimal;

public class PANDatabaseApp {

    private static final String HOSTNAME = "bopp0002-sql-server.database.windows.net";
    private static final String DBNAME = "cs-dsa-4513-sql-db";
    private static final String USERNAME = "bopp0002";
    private static final String PASSWORD = "abcdABCD@2001";
    private static final String CONNECTION_URL = "jdbc:sqlserver://" + HOSTNAME + ";database=" + DBNAME + ";user=" + USERNAME + ";password=" + PASSWORD;

    public static void main(String[] args) {
        try (Connection connection = DriverManager.getConnection(CONNECTION_URL)) {
            System.out.println("Connected to the database.");
            Scanner scanner = new Scanner(System.in);

            boolean running = true;
            while (running) {
                System.out.println("\nSelect an option:");
                System.out.println("1. Enter a new team");
                System.out.println("2. Enter a new client and associate with teams");
                System.out.println("3. Enter a new volunteer and associate with teams");
                System.out.println("4. Enter volunteer hours for a team");
                System.out.println("5. Enter a new employee and associate with teams");
                System.out.println("6. Enter an expense charged by an employee");
                System.out.println("7. Enter a new donor and donations");
                System.out.println("8. Retrieve doctor information of a client");
                System.out.println("9. Retrieve total expenses by each employee for a period");
                System.out.println("10. List volunteers for a client's teams");
                System.out.println("11. Retrieve teams founded after a specific date");
                System.out.println("12. Retrieve all people in the database");
                System.out.println("13. Retrieve employee donors with total donations");
                System.out.println("14. Increase salary of employees with multiple team reports");
                System.out.println("15. Delete clients without insurance and low transportation importance");
                System.out.println("16. Import: Enter new teams from a data file");
                System.out.println("17. Export: Retrieve names and mailing addresses of people on the mailing list");
                System.out.println("18. Quit");

                int choice = scanner.nextInt();
                scanner.nextLine();  

                switch (choice) {
                    case 1 -> enterNewTeam(connection, scanner);
                    case 2 -> enterNewClient(connection, scanner);
                    case 3 -> enterNewVolunteer(connection, scanner);
                    case 4 -> enterVolunteerHours(connection, scanner);
                    case 5 -> enterNewEmployee(connection, scanner);
                    case 6 -> enterExpense(connection, scanner);
                    case 7 -> enterNewDonor(connection, scanner);
                    case 8 -> retrieveClientDoctor(connection, scanner);
                    case 9 -> retrieveTotalExpensesByEmployee(connection, scanner);
                    case 10 -> listVolunteersForClientTeams(connection, scanner);
                    case 11 -> retrieveTeamsFoundedAfterDate(connection, scanner);
                    case 12 -> retrieveAllPeopleInfo(connection);
                    case 13 -> retrieveEmployeeDonors(connection);
                    case 14 -> increaseSalaryForMultiTeamEmployees(connection);
                    case 15 -> DeleteUninsuredClientsWithLowTransportValue(connection);
                    case 16 -> importTeamsFromFile(connection, scanner);
                    case 17 -> exportMailingListToFile(connection, scanner);
                    case 18 -> running = false;
                    default -> System.out.println("Invalid choice. Please try again.");
                }
            }
            System.out.println("Exiting the application.");
        } catch (SQLException e) {
            System.out.println("Error connecting to the database: " + e.getMessage());
        }
    }

    private static void enterNewTeam(Connection connection, Scanner scanner) {
        System.out.print("Enter team name: ");
        String teamName = scanner.nextLine();
        System.out.print("Enter team type: ");
        String teamType = scanner.nextLine();
        System.out.print("Enter formation date (yyyy-mm-dd): ");
        String date = scanner.nextLine();

        try (CallableStatement stmt = connection.prepareCall("{call sp_EnterNewTeam(?, ?, ?)}")) {
            stmt.setString(1, teamName);
            stmt.setString(2, teamType);
            stmt.setDate(3, Date.valueOf(date));
            stmt.execute();
            System.out.println("Team added successfully.");
        } catch (SQLException e) {
            System.out.println("Error adding team: " + e.getMessage());
        }
    }

    private static void enterNewClient(Connection connection, Scanner scanner) {
        System.out.print("Enter client SSN: ");
        String clientSSN = scanner.nextLine();
        System.out.print("Enter client name: ");
        String name = scanner.nextLine();
        System.out.print("Enter client gender (M/F): ");
        String gender = scanner.nextLine();
        System.out.print("Enter client profession: ");
        String profession = scanner.nextLine();
        System.out.print("Enter client mailing address: ");
        String mailingAddress = scanner.nextLine();
        System.out.print("Enter client phone number: ");
        String phoneNumber = scanner.nextLine();
        System.out.print("Enter client email ID: ");
        String emailID = scanner.nextLine();
        System.out.print("Is the client on the mailing list? (1 for yes, 0 for no): ");
        boolean onMailingList = scanner.nextInt() == 1;
        scanner.nextLine(); 
        System.out.print("Enter doctor name: ");
        String doctorName = scanner.nextLine();
        System.out.print("Enter date joined (yyyy-mm-dd): ");
        String dateJoined = scanner.nextLine();
        System.out.print("Enter doctor phone number: ");
        String doctorPhone = scanner.nextLine();
        System.out.print("Enter team name: ");
        String teamName = scanner.nextLine();

        try (CallableStatement stmt = connection.prepareCall("{call sp_EnterNewClient(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}")) {
            stmt.setString(1, clientSSN);               
            stmt.setString(2, name);                   
            stmt.setString(3, gender);                 
            stmt.setString(4, profession);              
            stmt.setString(5, mailingAddress);         
            stmt.setString(6, phoneNumber);            
            stmt.setString(7, emailID);                
            stmt.setBoolean(8, onMailingList);          
            stmt.setString(9, doctorName);              
            stmt.setDate(10, Date.valueOf(dateJoined)); 
            stmt.setString(11, doctorPhone);            
            stmt.setString(12, teamName);               
            stmt.execute();
            System.out.println("Client added and associated with team successfully.");
        } catch (SQLException e) {
            System.out.println("Error adding client: " + e.getMessage());
        }
    }



    private static void enterNewVolunteer(Connection connection, Scanner scanner) {
        System.out.print("Enter volunteer SSN: ");
        String volunteerSSN = scanner.nextLine();
        System.out.print("Enter volunteer name: ");
        String name = scanner.nextLine();
        System.out.print("Enter gender (M/F): ");
        String gender = scanner.nextLine();
        System.out.print("Enter profession: ");
        String profession = scanner.nextLine();
        System.out.print("Enter mailing address: ");
        String address = scanner.nextLine();
        System.out.print("Enter email: ");
        String email = scanner.nextLine();
        System.out.print("Enter phone number: ");
        String phone = scanner.nextLine();
        System.out.print("Is the volunteer on the mailing list? (1 for yes, 0 for no): ");
        int onMailingList = scanner.nextInt();
        scanner.nextLine();  

        System.out.print("Enter date joined (yyyy-mm-dd): ");
        String dateJoined = scanner.nextLine();
        System.out.print("Enter recent training date (yyyy-mm-dd): ");
        String trainingDate = scanner.nextLine();
        System.out.print("Enter recent training location: ");
        String trainingLocation = scanner.nextLine();
        System.out.print("Enter team name: ");
        String teamName = scanner.nextLine();
        System.out.print("Enter status (A for active, I for inactive): ");
        String status = scanner.nextLine();
        System.out.print("Enter hours worked: ");
        int hoursWorked = scanner.nextInt();
        scanner.nextLine();  

        try (CallableStatement stmt = connection.prepareCall("{call sp_AddPersonAndVolunteer(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}")) {
            stmt.setString(1, volunteerSSN);                
            stmt.setString(2, name);                        
            stmt.setString(3, gender);                      
            stmt.setString(4, profession);                  
            stmt.setString(5, address);                     
            stmt.setString(6, email);                       
            stmt.setString(7, phone);                       
            stmt.setInt(8, onMailingList);                  
            stmt.setDate(9, Date.valueOf(dateJoined));      
            stmt.setDate(10, Date.valueOf(trainingDate));   
            stmt.setString(11, trainingLocation);           
            stmt.setString(12, teamName);                   
            stmt.setString(13, status);                     
            stmt.setInt(14, hoursWorked);                   
            stmt.execute();
            System.out.println("Volunteer and person entries added successfully and associated with the team.");
        } catch (SQLException e) {
            System.out.println("Error adding volunteer: " + e.getMessage());
        }
    }


    private static void enterVolunteerHours(Connection connection, Scanner scanner) {
        System.out.print("Enter volunteer SSN: ");
        String volunteerSSN = scanner.nextLine();
        System.out.print("Enter team name: ");
        String teamName = scanner.nextLine();
        System.out.print("Enter additional hours worked: ");
        int hours = scanner.nextInt();
        System.out.print("Enter month: ");
        int month = scanner.nextInt();
        System.out.print("Enter status (A for active, I for inactive): ");
        String status = scanner.next();  
        scanner.nextLine();  

        try (CallableStatement stmt = connection.prepareCall("{call sp_EnterVolunteerHours(?, ?, ?, ?, ?)}")) {
            stmt.setString(1, volunteerSSN);  
            stmt.setString(2, teamName);     
            stmt.setInt(3, hours);            
            stmt.setInt(4, month);            
            stmt.setString(5, status);        
            stmt.execute();
            System.out.println("Volunteer hours updated successfully.");
        } catch (SQLException e) {
            System.out.println("Error updating volunteer hours: " + e.getMessage());
        }
    }



    private static void enterNewEmployee(Connection connection, Scanner scanner) {
        System.out.print("Enter employee SSN: ");
        String employeeSSN = scanner.nextLine();
        System.out.print("Enter employee name: ");
        String name = scanner.nextLine();
        System.out.print("Enter employee gender (M/F): ");
        String gender = scanner.nextLine();
        System.out.print("Enter employee profession: ");
        String profession = scanner.nextLine();
        System.out.print("Enter mailing address: ");
        String mailingAddress = scanner.nextLine();
        System.out.print("Enter email ID: ");
        String email = scanner.nextLine();
        System.out.print("Enter phone number: ");
        String phoneNumber = scanner.nextLine();
        System.out.print("Is the employee on the mailing list? (1 for yes, 0 for no): ");
        int onMailingList = scanner.nextInt();
        scanner.nextLine();  
        System.out.print("Enter marital status: ");
        String maritalStatus = scanner.nextLine();
        System.out.print("Enter hire date (yyyy-mm-dd): ");
        String hireDate = scanner.nextLine();
        System.out.print("Enter salary: ");
        double salary = scanner.nextDouble();
        scanner.nextLine();  
        System.out.print("Enter team name: ");
        String teamName = scanner.nextLine();
        System.out.print("Enter report date (yyyy-mm-dd): ");
        String reportDate = scanner.nextLine();
        System.out.print("Enter report description: ");
        String reportDescription = scanner.nextLine();

        try (CallableStatement stmt = connection.prepareCall("{call sp_EnterNewEmployee(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}")) {
            stmt.setString(1, employeeSSN);         
            stmt.setString(2, name);                
            stmt.setString(3, gender);             
            stmt.setString(4, profession);          
            stmt.setString(5, mailingAddress);      
            stmt.setString(6, email);               
            stmt.setString(7, phoneNumber);         
            stmt.setInt(8, onMailingList);          
            stmt.setString(9, maritalStatus);       
            stmt.setDate(10, Date.valueOf(hireDate)); 
            stmt.setDouble(11, salary);            
            stmt.setString(12, teamName);           
            stmt.setDate(13, Date.valueOf(reportDate)); 
            stmt.setString(14, reportDescription);  
            stmt.execute();
            System.out.println("Employee added and associated with team successfully.");
        } catch (SQLException e) {
            System.out.println("Error adding employee: " + e.getMessage());
        }
    }

    private static void enterExpense(Connection connection, Scanner scanner) {
        System.out.print("Enter employee SSN: ");
        String employeeSSN = scanner.nextLine();
        System.out.print("Enter expense date (yyyy-mm-dd): ");
        String expenseDate = scanner.nextLine();
        System.out.print("Enter amount: ");
        double amount = scanner.nextDouble();
        scanner.nextLine();
        System.out.print("Enter description: ");
        String description = scanner.nextLine();

        try (CallableStatement stmt = connection.prepareCall("{call sp_EnterExpense(?, ?, ?, ?)}")) {
            stmt.setString(1, employeeSSN);
            stmt.setDate(2, Date.valueOf(expenseDate));
            stmt.setDouble(3, amount);
            stmt.setString(4, description);
            stmt.execute();
            System.out.println("Expense entry created successfully.");
        } catch (SQLException e) {
            System.out.println("Error adding expense: " + e.getMessage());
        }
    }

    private static void enterNewDonor(Connection connection, Scanner scanner) {
        System.out.print("Enter donor SSN: ");
        String donorSSN = scanner.nextLine();
        System.out.print("Enter donor name: ");
        String name = scanner.nextLine();
        System.out.print("Enter donor gender (M/F): ");
        String gender = scanner.nextLine();
        System.out.print("Enter donor profession: ");
        String profession = scanner.nextLine();
        System.out.print("Enter mailing address: ");
        String mailingAddress = scanner.nextLine();
        System.out.print("Enter email ID: ");
        String email = scanner.nextLine();
        System.out.print("Enter phone number: ");
        String phoneNumber = scanner.nextLine();
        System.out.print("Is the donor on the mailing list? (1 for yes, 0 for no): ");
        boolean onMailingList = scanner.nextInt() == 1;
        System.out.print("Is the donor anonymous? (1 for yes, 0 for no): ");
        boolean anonymous = scanner.nextInt() == 1;
        System.out.print("Enter the number of donations: ");
        int donationCount = scanner.nextInt();
        scanner.nextLine(); 

        for (int i = 0; i < donationCount; i++) {
            System.out.println("Entering details for donation " + (i + 1));
            System.out.print("Enter donation type (Cheque/Credit Card): ");
            String donationType = scanner.nextLine();
            System.out.print("Enter campaign name: ");
            String campaignName = scanner.nextLine();
            System.out.print("Enter donation date (yyyy-mm-dd): ");
            String date = scanner.nextLine();
            System.out.print("Enter amount: ");
            double amount = scanner.nextDouble();
            scanner.nextLine(); 

            try (CallableStatement stmt = connection.prepareCall("{call sp_EnterNewDonor(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}")) {
                stmt.setString(1, donorSSN);
                stmt.setString(2, name);
                stmt.setString(3, gender);
                stmt.setString(4, profession);
                stmt.setString(5, mailingAddress);
                stmt.setString(6, phoneNumber);
                stmt.setString(7, email);
                stmt.setBoolean(8, onMailingList);
                stmt.setBoolean(9, anonymous);
                stmt.setInt(10, 1); 
                stmt.setString(11, donationType);
                stmt.setBigDecimal(12, BigDecimal.valueOf(amount));
                stmt.setDate(13, Date.valueOf(date));

                if ("Cheque".equalsIgnoreCase(donationType)) {
                    System.out.print("Enter cheque number: ");
                    stmt.setString(14, scanner.nextLine()); 
                    stmt.setNull(15, Types.CHAR); 
                    stmt.setNull(16, Types.VARCHAR); 
                    stmt.setNull(17, Types.DATE); 
                } else if ("Credit Card".equalsIgnoreCase(donationType)) {
                    System.out.print("Enter card number: ");
                    stmt.setNull(14, Types.CHAR); 
                    stmt.setString(15, scanner.nextLine()); 
                    System.out.print("Enter card type (e.g., Visa, MasterCard): ");
                    stmt.setString(16, scanner.nextLine()); 
                    System.out.print("Enter expiration date (yyyy-mm-dd): ");
                    stmt.setDate(17, Date.valueOf(scanner.nextLine())); 
                } else {
                    System.out.println("Invalid donation type.");
                    continue;
                }

                stmt.setString(18, campaignName); 
                stmt.execute();
                System.out.println("Donation added successfully.");
            } catch (SQLException e) {
                System.out.println("Error adding donation: " + e.getMessage());
            }
        }
    }




    private static void retrieveClientDoctor(Connection connection, Scanner scanner) {
        System.out.print("Enter client SSN: ");
        String clientSSN = scanner.nextLine();

        try (CallableStatement stmt = connection.prepareCall("{call sp_RetrieveClientDoctor(?)}")) {
            stmt.setString(1, clientSSN);
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                System.out.println("Doctor Name: " + rs.getString("doctor_name"));
                System.out.println("Doctor Phone Number: " + rs.getString("doctor_phone_number"));
            }
        } catch (SQLException e) {
            System.out.println("Error retrieving doctor information: " + e.getMessage());
        }
    }

    private static void retrieveTotalExpensesByEmployee(Connection connection, Scanner scanner) {
        System.out.print("Enter start date (yyyy-mm-dd): ");
        String startDate = scanner.nextLine();
        System.out.print("Enter end date (yyyy-mm-dd): ");
        String endDate = scanner.nextLine();

        try (CallableStatement stmt = connection.prepareCall("{call sp_TotalExpensesByEmployee(?, ?)}")) {
            stmt.setDate(1, Date.valueOf(startDate));
            stmt.setDate(2, Date.valueOf(endDate));
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                System.out.println("Employee SSN: " + rs.getString("employee_SSN"));
                System.out.println("Total Expenses: " + rs.getDouble("total_expenses"));
            }
        } catch (SQLException e) {
            System.out.println("Error retrieving total expenses: " + e.getMessage());
        }
    }

    private static void listVolunteersForClientTeams(Connection connection, Scanner scanner) {
        System.out.print("Enter client SSN: ");
        String clientSSN = scanner.nextLine();

        try (CallableStatement stmt = connection.prepareCall("{call sp_VolunteersForClientTeams(?)}")) {
            stmt.setString(1, clientSSN);
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                System.out.println("Volunteer SSN: " + rs.getString("SSN"));
                System.out.println("Recent Training Location: " + rs.getString("recent_training_location"));
            }
        } catch (SQLException e) {
            System.out.println("Error listing volunteers: " + e.getMessage());
        }
    }
 
    private static void retrieveTeamsFoundedAfterDate(Connection connection, Scanner scanner) {
        System.out.print("Enter date (yyyy-mm-dd): ");
        String date = scanner.nextLine();

        try (CallableStatement stmt = connection.prepareCall("{call sp_RetrieveTeamsFoundedAfterDate(?)}")) {
            stmt.setDate(1, Date.valueOf(date));
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                System.out.println("Team Name: " + rs.getString("Name"));
            }
        } catch (SQLException e) {
            System.out.println("Error retrieving teams: " + e.getMessage());
        }
    }

    private static void retrieveAllPeopleInfo(Connection connection) {
        try (CallableStatement stmt = connection.prepareCall("{call sp_RetrieveAllPeopleInfo()}")) {
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                System.out.println("Person Name: " + rs.getString("PersonName"));
                System.out.println("SSN: " + rs.getString("SSN"));
                System.out.println("Mailing Address: " + rs.getString("MailingAddress"));
                System.out.println("Phone Number: " + rs.getString("PhoneNumber"));
                System.out.println("Email ID: " + rs.getString("EmailID"));
                System.out.println("Emergency Contact Name: " + rs.getString("EmergencyContactName"));
                System.out.println("Emergency Contact Phone: " + rs.getString("EmergencyContactPhone"));
                System.out.println("Emergency Contact Relation: " + rs.getString("EmergencyContactRelation"));
                System.out.println("-----------------------------");
            }
        } catch (SQLException e) {
            System.out.println("Error retrieving people: " + e.getMessage());
        }
    }


    
    private static void retrieveEmployeeDonors(Connection connection) {
        try (CallableStatement stmt = connection.prepareCall("{call sp_RetrieveEmployeeDonors()}")) {
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                System.out.println("SSN: " + rs.getString("SSN"));
                System.out.println("Is Anonymous: " + rs.getBoolean("IsAnonymous"));
                System.out.println("Total Credit Donations: $" + rs.getDouble("TotalCreditDonation"));
                System.out.println("-----------------------------");
            }
        } catch (SQLException e) {
            System.out.println("Error retrieving employee donors: " + e.getMessage());
        }
    }

    
    private static void increaseSalaryForMultiTeamEmployees(Connection connection) {
        try (CallableStatement stmt = connection.prepareCall("{call sp_IncreaseSalaryForMultiTeamEmployees()}")) {
            stmt.execute();
            System.out.println("Salary updated successfully for eligible employees.");
        } catch (SQLException e) {
            System.out.println("Error updating salary: " + e.getMessage());
        }
    }

    
    private static void DeleteUninsuredClientsWithLowTransportValue(Connection connection) {
        try (CallableStatement stmt = connection.prepareCall("{call sp_DeleteUninsuredClientsWithLowTransportValue()}")) {
            stmt.execute();
            System.out.println("Clients without insurance and low importance for transportation were deleted.");
        } catch (SQLException e) {
            System.out.println("Error deleting clients: " + e.getMessage());
        }
    }

    

    private static void importTeamsFromFile(Connection connection, Scanner scanner) {
        System.out.print("Enter input file name for importing teams: ");
        String fileName = scanner.nextLine();

        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                if (values.length == 3) {
                    String teamName = values[0].trim();
                    String teamType = values[1].trim();
                    String formationDate = values[2].trim();

                    try (CallableStatement stmt = connection.prepareCall("{call sp_EnterNewTeam(?, ?, ?)}")) {
                        stmt.setString(1, teamName);
                        stmt.setString(2, teamType);
                        stmt.setDate(3, Date.valueOf(formationDate));
                        stmt.execute();
                        System.out.println("Team " + teamName + " added.");
                    } catch (SQLException e) {
                        System.out.println("Error adding team " + teamName + ": " + e.getMessage());
                    }
                } else {
                    System.out.println("Invalid line format: " + line);
                }
            }
            System.out.println("Import completed.");
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }

    private static void exportMailingListToFile(Connection connection, Scanner scanner) {
        System.out.print("Enter output file name for exporting mailing list: ");
        String fileName = scanner.nextLine();

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(fileName))) {
            String query = "SELECT name, mailing_address FROM Person WHERE on_mailing_list = 1";
            try (Statement stmt = connection.createStatement();
                 ResultSet rs = stmt.executeQuery(query)) {
                bw.write("Name,MailingAddress");
                bw.newLine();
                while (rs.next()) {
                    String name = rs.getString("name");
                    String mailingAddress = rs.getString("mailing_address");
                    bw.write(name + "," + mailingAddress);
                    bw.newLine();
                }
                System.out.println("Export completed.");
            } catch (SQLException e) {
                System.out.println("Database error: " + e.getMessage());
            }
        } catch (IOException e) {
            System.out.println("File writing error: " + e.getMessage());
        }
    }
}