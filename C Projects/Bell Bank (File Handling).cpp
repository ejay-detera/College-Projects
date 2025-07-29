#include<stdio.h>
#include<conio.h>
#include<process.h>
#include<stdlib.h>
#include<ctype.h>
#include<string.h>
#include<math.h>

struct account {//declares the structure account
    char account_no[10];
    char account_name[25];
    char PIN[6];
    double deposit;
    char transaction_code;
    double amount;
    double account_balance;
};

struct Credentials { //pre-defined account for the administrator
    char admin[10];
    char pin[6];
};
struct Credentials credentials = {"admin12", "12345"}; //provides value for the structure credentials

//declares functions
int menu();
void createAccount();
void login();
void AccountRegistered();
int available(char AccountNumber[]);
int availableName(char AccountName[]);
void submenu(char AccountNumber[]);
void withdraw(char AccountNumber[]);
void deposit(char AccountNumber[]);
void balance(char AccountNumber[]);
void changes(char AccountNumber[]);
int PINCHECKER(const char *pin);
void lockAccount(char AccountNumber[]);
void retrieveAccount();
void admin();
void adminmenu();

int main(){ //the file processing that we used is text mode.
	int choices;
    
	printf("\n\t\t\t[WELCOME TO BELL BANK]\n");
	printf("\n Note: Please press the corresponding number of the action you want to take.");
	printf("\n");
	do{
		choices = menu(); //returns the value of the menu function and gives it to the variable choices
	}while(choices != 3); //exits when it chooses 4(exit) in the menu function
	
	getch();
	return 0;
}

int menu(){//menu function
	int choices;
	
	printf("\n\n\t[Main Menu]\n\n");
    printf("  [1] Create an account | Sign up\n");
    printf("  [2] Open an account | Log In\n");
    printf("  [3] Administrator\n");
	printf("  [4] Exit\n");
    printf("\n\n  Enter your choice: ");
    while (scanf("%d", &choices)!= 1) {//loop until the user enters a valid integer
    printf("\nInvalid! DIGITS only.\n");// error message
        // Clear the input buffer
        int c;
        while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
        // to discard the input buffer
        return choices;
    }
    printf("\n");
	
	    switch(choices) { //calls the function based on the choice of the user
        case 1: createAccount(); break;
        case 2: login(); break;
        case 3: admin(); break;
        case 4: printf("\n  Thank you, have a nice day!\n\n"); printf("Exiting...\n"); return choices; 
        default: printf("  INVALID OPTION! Please enter a valid option.\n"); return 0;
    }
    return choices;
}

void createAccount(){//function that lets the user create accounts
	system("cls");// clears the screen
	char pin[6];
	FILE *AccountFile;
	
	AccountFile = fopen("ACCOUNTS.dat", "a+"); //a+ allows it to save even after the program is closed.

    if (AccountFile == NULL) {// prints if it cannot find the file ACCOUNTS.dat
        printf("File cannot be created!");
        return;
    }
    printf("[CREATE AN ACCOUNT | SIGN UP]");
    struct account ledger;
    printf("\nEnter Account number: ");
    scanf("%s", ledger.account_no);
    if(available(ledger.account_no) == 1){ //calls the available function to check on whether the account number that the user input is already in the file.
    	printf("\nAccount Already exist.");
    	fclose(AccountFile);
    	return;
	}
    printf("Enter Account Name: ");
    scanf("%s", ledger.account_name);
    strupr(ledger.account_name);
    if(availableName(ledger.account_name) == 1){ //calls the available function to check on whether the account number that the user input is already in the file.
    printf("\nAccount Already exist.");
    fclose(AccountFile);
    return;
	}
    printf("Enter PIN(6 DIGITS): ");
    scanf("%s", ledger.PIN);
    if(!PINCHECKER(ledger.PIN)){ //calls the function, if return 0 this will print
    	printf("\n[INVALID] PLEASE USE DIGITS ONLY.\n");
    	menu();
    	return;
	}
	 if (strlen(ledger.PIN) != 6) { //if the length of the PIN is not equal to 6, it prints
        printf("\n[INVALID] PLEASE ENTER 6 DIGITS\n");
        menu();
        return;
    }
    do{
    	
    printf("Enter Deposit: ");
    //checks for any special character or alphabet that the user inputs
    while (scanf("%lf", &ledger.deposit) != 1) {//loop until the user enters a valid integer
    printf("\nInvalid! DIGITS only.\n");//prints if there are any special characters or alphabet 
    // Clear the input buffer
        int c;
        while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
        // to discard the input buffer
        return;
    }
    if(ledger.deposit <= 0){ //Ensures so that the user cannot put negative numbers
	  printf("\nInvalid Deposit!");
	  return;
	}
	}while(ledger.deposit <=0);
		ledger.account_balance = ledger.deposit;
		//prints the data to the file
    fprintf(AccountFile, "%s %s %s %.2lf %.2lf\n", ledger.account_no, ledger.account_name, ledger.PIN, ledger.deposit, ledger.account_balance);

    fclose(AccountFile);
    menu();
    return;
}

void login(){//login function
	system("cls");// clears the screen
	char AccountNumber[10];
	char pin[6];
	
	FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); //reads the file ACCOUNTS.dat

    if (AccountFile == NULL) {
        printf("File cannot be read!");
        return;
    }
    printf("[OPEN AN ACCOUNT | LOG IN]");
	printf("\nEnter your Account Number: ");
	scanf("%s", AccountNumber);
	
	struct account temp;
	int checker = 0;
	//checks the whole file until its EOF(End of File)
	while (fscanf(AccountFile, "%s %s %s %lf %lf", temp.account_no, temp.account_name, temp.PIN, &temp.deposit, &temp.account_balance) != EOF) {
    if (strcmp(temp.account_no, AccountNumber) == 0) {//compares the user input Account Number with the account number in the file.
        checker = 1; //if the account matches, checker =1 which means true.
        break;
        }
    }
    fclose(AccountFile);
    
    if(checker == 0){//prints if the user input Account Number does not match with the account number in the file.
    	printf("\nAccount is not found!");
		return;
	}
	
	int attempts = 3; //gives the user 3 attempts
	while(attempts > 0){ //ends if the user used up all its attempts
		printf("Enter PIN: ");
		scanf("%6s", pin); // %6s to prevent buffer
	if(!PINCHECKER(pin)){ //calls the function, if return 0 this will print
    	printf("\n[INVALID] PLEASE USE DIGITS ONLY.\n");
    	menu();
    	return;
	}
	if (strlen(pin) != 6) {
        printf("\n[INVALID] PLEASE ENTER 6 DIGITS\n");
        menu();
        return;
    }
	if(strcmp(temp.PIN, pin) == 0){ //compares the pin inside the file and the user input pin.
		printf("\n [VALID PIN]\n");
		printf("\nWELCOME, ACCOUNT[%s].\n\n", temp.account_name); //welcomes the user
		submenu(AccountNumber);//calls the submenu function
		return;
	}else{
	    	printf("\nINVALID PIN.\n");
	    	printf("[%d] ATTEMPTS LEFT.\n", --attempts); //decrements attempt every time you failed to put your pin.
		if(attempts == 0){
			    lockAccount(AccountNumber);//locks the account when you exceed the amount of attempts
				printf("\n[CARD CAPTURED]\n"); //prints
				printf("Please get your card from your nearest BELL Bank Branch.\n");
		}
	}
	}
	menu();
    return;
}
//available only in admin menu function
void AccountRegistered() { //view all registered accounts function
    FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); // open file for reading

    if (AccountFile == NULL) {
        printf("File cannot be read!");
        return;
    }

    struct account temp;
    printf("o--o--o--o--o--o--o--o--o--o--o--o--o--o--o--o--o");
    printf("\n\n  ACCOUNT NO.\tACCOUNT NAME\t\tBALANCE\t\n\n");
    // prints the data inside the file ACCOUNTS.dat until its EOF(End of File).
    while (fscanf(AccountFile, "%s %s %s %lf %lf\n", temp.account_no, temp.account_name, temp.PIN, &temp.deposit, &temp.account_balance) != EOF) {
        printf("  %-10s\t%-20s\t $%.2lf\n", temp.account_no, temp.account_name, temp.account_balance);
    }
    printf("\n");
    printf("o--o--o--o--o--o--o--o--o--o--o--o--o--o--o--o--o");
    fclose(AccountFile);
    
    adminmenu(); //goes back to admin menu function
    return;
}
	

void submenu(char AccountNumber[]){//submenu or transaction menu
	FILE *AccountFile;
	if (AccountFile == NULL) {
        printf("File cannot be read!");
        return;
    }
    struct account temp;
    printf("\n\t[SUB-MENU]\n");
    printf("\nEnter Transaction Code: <B>, <D>, <W>, <E>\n\n");
    printf("[B] BALANCE INQUIRY [D] DEPOSIT [W] WITHDRAW [C] C. ACCOUNT [E] EXIT\n\n");
    printf("Enter your choice: ");
    scanf(" %c", &temp.transaction_code);
    temp.transaction_code = toupper(temp.transaction_code);//changes lower case to upper case
    
    if(isalpha(temp.transaction_code) == 0){//checks if the user inputs a number instead of an alphabet
    	printf("\nERROR! WRONG TRANSACTION CODE.");//prints if the user inputs a number
    	return;//return to the main menu
	}
	
    switch(temp.transaction_code) {
        case 'B': balance(AccountNumber); break;
        case 'D': deposit(AccountNumber); break;
        case 'W': withdraw(AccountNumber); break;
        case 'C': changes(AccountNumber); break;
        case 'E': menu(); return; 
        
        default: printf("\n\nINVALID TRANSACTION.\n"); submenu(AccountNumber); return;
    }
    system("cls");//clear screen
    menu();
    return;
}
//available only in submenu
void withdraw(char AccountNumber[]){//withdraw function
	int num = 0;
	struct account list[100];//value of 100 to ensure that every account can be checked
	
	FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); //opens the file to be read

    if (AccountFile == NULL) {
        printf("File cannot be read nor write!");
        return;
    }
    //checks every data in the file
        while (fscanf(AccountFile, "%s %s %s %lf %lf\n", list[num].account_no, list[num].account_name, list[num].PIN , &list[num].deposit, &list[num].account_balance) != EOF) {
        num++;//iterates, counts the amount of data inside the file
    }
    printf("\n\n[WITHDRAW]");
    int checker = 0;
    double check = 10; //used in fmod
    for(int i= 0; i<num; i++){
    	if(strcmp(list[i].account_no, AccountNumber) == 0){
    		checker = 1;
    		printf("\n\nNOTE: Minimum withdrawal amount of $100 and maximum of $50k.\n");
    		printf("\nEnter amount to withdraw: $");
    		//checks for any special character or alphabet that the user inputs
    	    while (scanf("%lf", &list[i].amount) != 1) {//loop until the user enters a valid integer
            printf("\nInvalid! DIGITS only.\n");//prints if there are any special characters or alphabet 
            // Clear the input buffer
            int c;
            while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
            // to discard the input buffer
            submenu(AccountNumber);
            return;
    }
    		if(list[i].account_balance >= list[i].amount){ 
    			if(fmod(list[i].amount, check) == 0 && list[i].amount >= 100 && list[i].amount <= 50000){ //divides the list[i].amount by the check and 
				//if the value is 0 is greater than 100 and does not exceed 50k, it proceeds.
				
    				list[i].account_balance -= list[i].amount; //minuses the account balance and puts the result again to the account balance.
    			printf("\n [WITHDRAW SUCCESSFUL].\n");
    			printf("BALANCE: $%.2lf\n", list[i].account_balance);//shows the balance after the transaction
				}else if(list[i].amount > 50000){ //prints if value of withdrawal exceeds the maximum amount
					printf("\n [INVALID] EXCEEDED THE MAXIMUM AMOUNT OF WITHDRAWAL.\n");
				}
				 else{
					printf("\nThe amount you want to withdraw is not valid.\n");//if the result of fmod is not equal to 0 nor greater than or equal to 0 it prints this.
				}
    			
			}else{
				printf("\nINVALID! NOT ENOUGH FUNDS.\n");//if the account balance is less than the amount you want to withdraw, it prints this
			}
			
			break;
		}
	}
    if(checker == 0){// prints if the accounts is not found
    	printf("\nAccount is not found!");
		fclose(AccountFile);
		return;
	}

	fclose(AccountFile);
    AccountFile = fopen("ACCOUNTS.dat", "w");//opens the file for writing

    if (AccountFile == NULL) {
        printf("File cannot be opened for writing!");
        return;
    }
	
	for(int i=0; i<num;i++){ //rewrites the data inside the file based on the changes we did in this function.
		fprintf(AccountFile, "%s %s %s %.2lf %.2lf\n", list[i].account_no, list[i].account_name, list[i].PIN ,list[i].deposit, list[i].account_balance);
	}
	
    fclose(AccountFile);
    submenu(AccountNumber);
    return; //returns to submenu
}

void deposit(char AccountNumber[]){//deposit function
	printf("\n\n[DEPOSIT]");
	int num = 0;
	struct account list[100];//value of 100 to ensure that every account can be checked
	
	FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); //opens file for reading

    if (AccountFile == NULL) {
        printf("File cannot be read nor write!");
        return;
    }
    //checks every data in the file
        while (fscanf(AccountFile, "%s %s %s %lf %lf\n", list[num].account_no, list[num].account_name, list[num].PIN ,&list[num].deposit, &list[num].account_balance) != EOF) {
        num++; //iterates, counts the amount of data inside the file
    }
    
    int checker = 0;
    for(int i= 0; i<num; i++){
    	if(strcmp(list[i].account_no, AccountNumber) == 0){
    		checker = 1;
    		printf("\nEnter amount to deposit: $");//asks the user for the amount to deposit
    		//checks for any special character or alphabet that the user inputs
    	    while (scanf("%lf", &list[i].amount) != 1) {//loop until the user enters a valid integer
            printf("\nInvalid! DIGITS only.\n");//prints if there are any special characters or alphabet 
            // Clear the input buffer
            int c;
            while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
            // to discard the input buffer
            submenu(AccountNumber);
        	return;
    }
    		list[i].account_balance += list[i].amount; //adds amount and account balance, then puts it again in account balance
    			printf("\n [DEPOSIT SUCCESSFUL!]\n");
    			printf("BALANCE: $%.2lf\n", list[i].account_balance); //prints the current balance
		}
	}
	
    if(checker == 0){ //prints if account is not found
    	printf("\nAccount is not found!");
		fclose(AccountFile);
		return submenu(AccountNumber);
	}

	fclose(AccountFile);
    AccountFile = fopen("ACCOUNTS.dat", "w"); //opens file for writing

    if (AccountFile == NULL) {
        printf("File cannot be opened for writing!");
        return;
    }
	
	for(int i=0; i<num;i++){//rewrites the data inside the file based on the changes we did in this function.
		fprintf(AccountFile, "%s %s %s %.2lf %.2lf\n", list[i].account_no, list[i].account_name, list[i].PIN ,list[i].deposit, list[i].account_balance);
	}
	
    fclose(AccountFile);
    submenu(AccountNumber);
    return; //returns to sub menu
}

int available(char AccountNumber[]){ //checks the account number if it is available function
	int checker = 0; //false
	FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); //opens file for reading

    if (AccountFile == NULL) { //prints if the file ACCOUNTS.dat does not exist
        printf("File cannot be read!");
        return 0;
    }
       struct account temp;
       //checks every data in the file until EOF(End of File)
    while (fscanf(AccountFile, "%s %s %s %lf %lf\n", temp.account_no, temp.account_name, temp.PIN ,&temp.deposit, &temp.account_balance) != EOF) {
        if(strcmp(temp.account_no, AccountNumber) == 0){ //compares the Account Number the user input to the account number inside the file
        	fclose(AccountFile);
        	checker = 1; //true, the account number already exists within the file
        	return checker; //returns value of checker
		}
    }
    
    fclose(AccountFile);
    return checker; //returns value of checker
}

int availableName(char AccountName[]){ //checks the account number if it is available function
	int checker = 0; //false
	FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); //opens file for reading

    if (AccountFile == NULL) { //prints if the file ACCOUNTS.dat does not exist
        printf("File cannot be read!");
        return 0;
    }
       struct account temp;
       //checks every data in the file until EOF(End of File)
    while (fscanf(AccountFile, "%s %s %s %lf %lf\n", temp.account_no, temp.account_name, temp.PIN ,&temp.deposit, &temp.account_balance) != EOF) {
        if(strcmp(temp.account_name, AccountName) == 0){ //compares the Account Number the user input to the account number inside the file
        	fclose(AccountFile);
        	checker = 1; //true, the account number already exists within the file
        	return checker; //returns value of checker
		}
    }
    
    fclose(AccountFile);
    return checker; //returns value of checker
}

void balance(char AccountNumber[]){ //balance inquiry function
	system("cls"); //clears screen
	FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); //opens file for reading

    if (AccountFile == NULL) {
        printf("File cannot be read!");
        return;
    }
    
    struct account temp;
    //prints until EOF(End of File)
    while (fscanf(AccountFile, "%s %s %s %lf %lf\n", temp.account_no, temp.account_name, temp.PIN ,&temp.deposit, &temp.account_balance) != EOF) {
    	if(strcmp(temp.account_no, AccountNumber) == 0){
    		printf("\n[BALANCE INQUIRY]");
    		printf("\n\n[ACCOUNT NO.]: %s", temp.account_no);
    		printf("\n[ACCOUNT NAME]: %s", temp.account_name);
    		printf("\n[BALANCE]: %.2lf", temp.account_balance);
    		printf("\n");
		}
    }
    fclose(AccountFile);
    submenu(AccountNumber); //returns to the submenu
    return;
}

void changes(char AccountNumber[]) { //changes in account, for the user to check on what changed in his account
    FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); //opens file for reading
    if (AccountFile == NULL) {
        printf("File cannot be read!");
        return;
    }
    
    struct account temp;
    //prints until EOF(End of File)
    while (fscanf(AccountFile, "%s %s %s %lf %lf\n", temp.account_no, temp.account_name, temp.PIN ,&temp.deposit, &temp.account_balance) != EOF) {
    	if(strcmp(temp.account_no, AccountNumber) == 0){
    		printf("\n[CHANGES IN ACCOUNT]");
			printf("\n\n  ACCOUNT NO.\tACCOUNT NAME\tORIGINAL BALANCE\tCURRENT BALANCE\t\n\n");
			printf("  %s\t\t %s \t\t $%.2lf \t\t $%.2lf\n", temp.account_no, temp.account_name,temp.deposit, temp.account_balance);
    		printf("\n");
		}
    }


    fclose(AccountFile);
    submenu(AccountNumber); //returns to the submenu
    return;
}

int PINCHECKER(const char *pin){ //function that checks if the pin are all digits
	 while (*pin) {//loop through each character in the string until '\0' (end of string) is encountered
        if (!isdigit(*pin)) {
            return 0; //return 0 if any non-digit character is found
        }
        pin++; //move to the next character in the string
    }
    return 1; //return 1 if all characters in the string are digits
}

void lockAccount(char AccountNumber[]) {//lock an account function
    FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); //open file for reading
    FILE *TempFile = fopen("TEMP.dat", "w"); // open temporary file for writing

    if (AccountFile == NULL || TempFile == NULL) {
        printf("File cannot be read or written!");
        return;
    }

    struct account temp;
    int found = 0;

    // read the file and find the account to lock
    while (fscanf(AccountFile, "%s %s %s %lf %lf\n", temp.account_no, temp.account_name, temp.PIN, &temp.deposit, &temp.account_balance)!= EOF) {
        if (strcmp(temp.account_no, AccountNumber) == 0) {
            found = 1;
            //lock the account by rewriting the PIN with locked
            strcpy(temp.PIN, "LOCKED");
            strcpy(temp.account_no, "[LOCKED]");
        }
        //prints the new data into the file
        fprintf(TempFile, "%s %s %s %.2lf %.2lf\n", temp.account_no, temp.account_name, temp.PIN, temp.deposit, temp.account_balance);
    }

    fclose(AccountFile);
    fclose(TempFile);

    // replace the original file with the temporary file
    remove("ACCOUNTS.dat");
    rename("TEMP.dat", "ACCOUNTS.dat");

    if (found) { //if the account is found, it rewrties the value of account number with locked
        printf("\n[ACCOUNT LOCKED]\n");
    } else {
        printf("\n[ACCOUNT NOT FOUND]\n");
    }
    return;
}

void retrieveAccount() {
	system("cls"); //clears screen
	char AccountName[25];
	printf("\n[RETRIEVE ACCOUNT]\n"); //asks the user for the account name, since we changed the vlaue of the pin and account number to LOCKED
	printf("\nEnter Account Name: "); 
	scanf("%s", AccountName); 
	strupr(AccountName); //uppercases the string
    FILE *AccountFile = fopen("ACCOUNTS.dat", "r"); // open file for reading
    FILE *TempFile = fopen("TEMP.dat", "w"); // open temporary file for writing

    if (AccountFile == NULL || TempFile == NULL) { //prints if both file does not exist
        printf("File cannot be read or written!");
        return;
    }

    struct account temp;
    int found = 0; //false

    //read the file and find the account to retrieve
    while (fscanf(AccountFile, "%s %s %s %lf %lf\n", temp.account_no, temp.account_name, temp.PIN, &temp.deposit, &temp.account_balance)!= EOF) {
        if (strcmp(temp.account_name, AccountName) == 0 && strcmp(temp.PIN, "LOCKED") == 0) { //compares account name,and pin
		//to ensure that the account was really locked
		
            found = 1; //true
            //asks the admin for new account number and pin
            printf("Enter new Account Number: ");
            scanf("%s", temp.account_no);
            if(available(temp.account_no) == 1){ //calls the available function to check on whether the account number that the user input is already in the file.
    	    printf("\nAccount Already exist.");
    	    fclose(AccountFile);
    	return;
			}
            printf("Enter new PIN: ");
            scanf("%s", temp.PIN);
            printf("\n[ACCOUNT RETRIEVED]\n");
        }
        //prints the new data into the file
        fprintf(TempFile, "%s %s %s %.2lf %.2lf\n", temp.account_no, temp.account_name, temp.PIN, temp.deposit, temp.account_balance);
    }
    
    //closes both file
    fclose(AccountFile);
    fclose(TempFile);

    // replaces the original file with the temporary file
    remove("ACCOUNTS.dat");
    rename("TEMP.dat", "ACCOUNTS.dat");

    if (!found) { //prints if the account is not found
        printf("\n[ACCOUNT NOT FOUND OR NOT LOCKED]\n");
    }
    adminmenu();
    return;
}

void adminmenu(){//admin menu function
int choices;
			 
            printf("\n\n\t[Admin Menu]\n\n");
            printf("  [1] View Listed Accounts\n");
            printf("  [2] Retrieve Locked Account\n");
            printf("  [3] Exit\n");
            printf("\n\n  Enter your choice: ");
            while (scanf("%d", &choices) != 1) {//loop until the user enters a valid integer
                printf("\nInvalid! DIGITS only.\n");// error message
                // Clear the input buffer
                int c;
                while ((c = getchar()) != '\n' && c != EOF); //reads until EOF(End of File) or '\n' (end of line)
                // to discard the input buffer
            adminmenu();
            return;
            }
            printf("\n");
            
            switch(choices) { //calls the function based on the choice of the user
                case 1: AccountRegistered(); break;
                case 2: retrieveAccount(); break;
                case 3: system("cls"); printf("Exiting Admin Mode.\n"); menu(); //returns to the menu function
                default: printf("  INVALID OPTION! Please enter a valid option.\n"); return;
            }
    system("cls");//clears screen
    adminmenu();
    return;
}

void admin(){//admin log in function
    int choices;
    char Admin[10];
    char pin[6];

    
    printf("Enter Admin Account: ");
    scanf("%9s", Admin); //use %9s to prevent buffer overflow
    if(strcmp(credentials.admin, Admin) == 0){//compares the predefined account with the user input data
        printf("Enter PIN: ");
        scanf("%5s", pin); //use %5s to prevent buffer overflow
        if(PINCHECKER(pin) == 1){ //calls the function PINCHECKER to check if the pin is a digit or not
        if(strcmp(credentials.pin, pin) == 0){ //compares the predefined account with the user input data
		adminmenu(); //calls the admin menu function
		return;
		}else{ //prints if the pin is invalid and returns back to the menu function
			printf("Invalid PIN. Please try again.\n");
			menu();
			return;
		}
		}else{ //prints if the pin contains special character or alphabets
			printf("[INVALID] PLEASE USE DIGITS ONLY. \n");
			menu();
			return;
		}
	}else{//prints if the admin account is invalid and returns back to the menu function
		printf("Invalid Admin Account. Please try again.\n");
			menu();
			return;
		}
	menu();
    return;
}
