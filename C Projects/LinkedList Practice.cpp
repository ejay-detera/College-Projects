#include<stdio.h>
#include<conio.h>
#include<malloc.h>

struct node{
    struct node *prev;
    int x;
    struct node *next;
};

struct node *head, *curr, *tail;

int main(){

    bool checker = true;
    int ctr = 0;

   // make head, curr, and tail's value equals to NULL
    head = curr = tail = NULL;
    // allocate the first memory to curr
    curr = (struct node*)malloc(sizeof(struct node));
    // ask input
    printf("Enter an integer value (0) for exit: ");
    scanf("%d", &curr->x);

    // loop for value entry and create memory allocation for curr each iteration
    while(curr-> x != 0){
        if(head == NULL){ // point head, curr, and tail into one memory allocation (1 time)
            head = curr;
            head->prev = NULL;
            head->next = NULL;
            tail = curr;
            ctr++;
        }
        else{ // reallocate curr and tail to its new memory allocation each interation
            tail->next = curr;
            curr->prev = tail;
            curr->next = NULL;
            tail = curr;
            ctr++;
        }
        // create a new memory allocation for curr
        curr = (struct node*)malloc(sizeof(struct node));

        printf("Enter an integer value (0) for exit: ");
        scanf("%d", &curr->x);
    }
    // check if there's no input from user at all
    if(head == NULL){
        printf("No data entered!");
    }

    struct node *headCheck, *tailCheck;

    headCheck = head; // headChecker will start from head
    tailCheck = tail; // tailChecker will start from tail

    // will loop based on the number of nodes created with a value in it
    for(int i = 0; i < ctr; i++){
        if(headCheck->x != tailCheck->x){ // check if both side are not equal and consider automatically not a palindrome
            checker = false;
            break;
        }
        else{
            headCheck = headCheck->next;
            tailCheck = tailCheck->prev;
        }
    }

    if(checker){
        printf("Doubly linked list is a palindrome!");
    }
    else{
        printf("Doubly linked list is not a palindrome!");
    }

    return 0;
}
