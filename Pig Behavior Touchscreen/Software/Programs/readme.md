# The programs developed so far are provided here

### Shaping_full.py - should be starting point for training pigs to respond
The "auto-shaping" feature will help, but some form of hand-shaping responses may be needed. Currently "ctrl+R" on a keyboard will deliver reinforcers, allowing handshaping.
There is a known bug in that this "ctrl+R" response stops working and we have not figured out the root cause.

### Motor_Task_Acc.py - a shrinking box titrates to pig accuracy
FR-3 is recommended so pigs can zero in on the response box. FR-1 may result in too few reinforcers earned and actually decrease ability to perform at high levels

### Motor_Task_ReactionTime.py - a response to one box populates a different box in another corner. The time to respond titrates based on accuracy.
This task is set to FR-1. 
To do/updates: Recommend setting up program from random to pseudorandom for arranging the types of trials (left/right, up/down, diagonal), potentially each with its own independent timer. 
This will allow the user to parcellate "harder" trials in data analysis (down/diagonal are more difficult in observations) and make sure you obtain a balanced set of trial types.
