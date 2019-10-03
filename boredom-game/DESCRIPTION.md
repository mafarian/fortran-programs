Hey!

Choose a finite list of random natural numbers (repeated numbers are allowed), like

> 1 5 2 2 1 9 8 4 3 7 12 3 5 11       
>> points = 0

Then, you can pick a number **x** to earn **x** points, BUT the numbers **x-1** and **x+1** will be deleted from the list. For example, if you choose **8**:

> 1 5 2 2 1 ~~9~~ **8** 4 3 ~~7~~ 12 3 5 11     
>> points = 8

And so on, until there's no left numbers to choose

> 1 5 2 2 1 ~~9~~ **8** 4 3 ~~7~~ **12** 3 5 ~~11~~
>> points = 8+12 = 20
>
> 1 **5** 2 2 1 ~~9~~ **8** ~~4~~ 3 ~~7~~ **12** 3 **5** ~~11~~
>> points = 20+5+5 = 30
>
> ~~1~~ **5** **2** **2** ~~1~~ ~~9~~ **8** ~~4~~ ~~3~~ ~~7~~ **12** ~~3~~ **5** ~~11~~ 
>> points = 30+2+2 = 34

We liked to make the best move, ie, a sequence of decisions that will result in the greatest score! We solved that problem using bottom-up Dynamic Programming. 


-- *Stark*.
