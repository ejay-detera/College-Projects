#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>


struct node {
    int x;
    struct node* next;
    struct node* prev;
};

struct node* head = NULL;
struct node* curr = NULL;
struct node* tail = NULL;

char menu();
void create();
void display();
void insert();
void deleteNode();
void search();

int linkedlist = 0;
int main() {
    char choice;
    do {
        choice = menu();
    } while (toupper(choice) != 'F');

    printf("\nExiting program. Thank you for using!\n");
    return 0;
}

char menu() {
    char choice;
    printf("\n\n\t[Main Menu]\n\n");
    printf("  [A] Create a Linear Linked List\n");
    printf("  [B] Display a Linear Linked List\n");
    printf("  [C] Insert a number in the Linear Linked List\n");
    printf("  [D] Delete a number from the Linear Linked List\n");
    printf("  [E] Search a number\n");
    printf("  [F] Exit\n");
    printf("\n\n  Enter your choice: ");
    scanf(" %c", &choice);
    printf("\n");
    choice = toupper(choice);

switch (choice) {
        case 'A': create(); break;
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        if(linkedlist == 1){
		if (choice == 'B') display();
        else if (choice == 'C') insert();
        else if (choice == 'D') deleteNode();
        else if (choice == 'E') search();
		}else{
			printf("\nPlease create a Linkedlist(option A) first before choosing among the options.\n");
		} break;
        case 'F': return 'F';
        default: printf("\nInvalid option! Please select between provided options\n"); break;
    }
    return choice;
}

void create() {
    struct node* temp;
    int value;

    head = curr = tail = NULL;
    printf("Enter an integer value (enter 0 to stop): ");
    while (scanf("%d", &value)!= 1) {//loop until the user enters a valid integer
        printf("\nInvalid! DIGITS only.\n");// error message
        printf("\nEnter an integer value (enter 0 to stop): ");//asks the user again to input an integer
        // Clear the input buffer
        int c;
        while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
        // to discard the input buffer
    }
    while (value != 0) {
        curr = (struct node*)malloc(sizeof(struct node));
        curr->x = value;
        curr->next = NULL;
        curr->prev = tail;

        if (head == NULL) {
            head = curr;
        } else {
            tail->next = curr;
        }
        tail = curr;

    printf("Enter an integer value (enter 0 to stop): ");
    while (scanf("%d", &value)!= 1) {//loop until the user enters a valid integer
        printf("\nInvalid! DIGITS only.\n");// error message
        printf("\nEnter an integer value (enter 0 to stop): ");//asks the user again to input an integer
        // Clear the input buffer
        int c;
        while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
        // to discard the input buffer
    }
    }

    // Sorting the linked list
    struct node* i = head;
    while (i != NULL) {
        struct node* j = i->next;
        while (j != NULL) {
            if (i->x > j->x) {
                int tempValue = i->x;
                i->x = j->x;
                j->x = tempValue;
            }
            j = j->next;
        }
        i = i->next;
    }

    linkedlist = 1;
}

void display() {
    curr = head;
    while (curr != NULL) {
        printf("%3d ", curr->x);
        curr = curr->next;
    }
    printf("\n");
}

void insert() {
    char choice;
    do {
        int data;
        printf("Enter the number to insert: ");
        while (scanf("%d", &data)!= 1) {//loop until the user enters a valid integer
        printf("\nInvalid! DIGITS only.\n");// error message
        // Clear the input buffer
        int c;
        while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
        // to discard the input buffer
        break;
    }


        struct node* newNode = (struct node*)malloc(sizeof(struct node));
        newNode->x = data;
        newNode->next = NULL;
        newNode->prev = NULL;

        if (head == NULL || head->x >= data) {
            newNode->next = head;
            if (head != NULL) {
                head->prev = newNode;
            }
            head = newNode;
            if (tail == NULL) {
                tail = newNode;
            }
        } else {
            struct node* temp = head;
            while (temp->next != NULL && temp->next->x < data) {
                temp = temp->next;
            }

            newNode->next = temp->next;
            newNode->prev = temp;
            if (temp->next != NULL) {
                temp->next->prev = newNode;
            }
            temp->next = newNode;
            if (newNode->next == NULL) {
                tail = newNode;
            }
        }
        printf("Number %d inserted into the list.\n", data);
        while(1){
        printf("Do you want to insert another number (Y/N)? ");
        scanf(" %c", &choice);
        choice = toupper(choice);
        if(choice == 'Y' || choice == 'N') {
            break;
        }else{
              printf("Wrong command. Please enter 'Y' or 'N'.\n\n");}
        }
    } while (choice == 'Y');
}

void deleteNode() {
    char choice;
    do {
        int data;
        int found=0;
        while (scanf("%d", &data)!= 1) {//loop until the user enters a valid integer
        printf("\nInvalid! DIGITS only.\n");// error message
        // Clear the input buffer
        int c;
        while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
        // to discard the input buffer
        break;
    }

        struct node* temp = head;
        while (temp != NULL) {
            if (temp->x == data) {
                found = 1;
                struct node* toDelete = temp;
                if (toDelete->prev != NULL) {
                    toDelete->prev->next = toDelete->next;
                } else {
                    head = toDelete->next;
                }

                if (toDelete->next != NULL) {
                    toDelete->next->prev = toDelete->prev;
                } else {
                    tail = toDelete->prev;
                }

                temp = toDelete->next;
                free(toDelete);
            } else {
                temp = temp->next;
            }
        }
          if (found) {
            printf("Number %d deleted from the list.\n", data);
        } else {
            printf("Number %d not found in the list.\n", data);
        }
        while(1){
        printf("Do you want to delete another number (Y/N)? ");
        scanf(" %c", &choice);
        choice = toupper(choice);
        if (choice == 'Y' || choice == 'N') {
            break;
        }else{
            printf("Wrong command. Please enter 'Y' or 'N'.\n\n");
        }
        }
    } while (choice == 'Y');
}
void search() {
    int num;
    char choice;
    if(linkedlist == 0){
    	printf("\nPlease create a Linkedlist(option A) first.\n");
    	return;
	}
    do {
        int found = 0;
        printf("Enter a number to search: ");
        while (scanf("%d", &num)!= 1) {//loop until the user enters a valid integer
        printf("\nInvalid! DIGITS only.\n");// error message
        // Clear the input buffer
        int c;
        while ((c = getchar())!= '\n' && c!= EOF); //reads until EOF(End of File) or '\n' (end of line)
        // to discard the input buffer
        break;
    }


        struct node* temp = head;
        while (temp != NULL) {
            if (temp->x == num) {
                found = 1;
                break;
            }
            temp = temp->next;
        }

        if (found == 1) {
            printf("\n%d is in the list\n", num);
        } else {
            printf("\n%d is not in the list\n", num);
        }
        while(1){
        printf("Do you want to search for another number (Y/N)? ");
        scanf(" %c", &choice);
        choice = toupper(choice);
        if (choice == 'Y' || choice == 'N') {
        break;
        }else{
    printf("Wrong command. Please enter 'Y' or 'N'.\n\n");
}
        }
    } while (choice != 'N');
}

