              *> NOT, negating a conditional
              MOVE 50 TO NUM1.
              MOVE 60 TO NUM2.
              IF NOT NUM2 IS LESS THAN NUM1 THEN
                DISPLAY NUM2' IS NOT LESS THAN 'NUM1
              END-IF

              *> AND, having multiple conditionals
              IF NUM1 IS LESS THAN NUM2 AND NUM1 IS LESS THAN 100 THEN
                DISPLAY 'COMBINED CONDITION'
              ELSE
                DISPLAY 'NAH'
              END-IF
      
              *> checking for negative or positive values
              IF NEG-NUM IS POSITIVE OR NEG-NUM IS NEGATIVE THEN
                DISPLAY 'A NUMBER IS POSITIVE'.
      
              *> checking for negative or positive values
              IF NEG-NUM IS NEGATIVE THEN
                DISPLAY 'A NUMBER IS NEGATIVE'.

              *> checking if a variable is a certain
              *> data type
              IF CLASS1 IS ALPHABETIC OR CLASS1 IS NUMERIC THEN
                DISPLAY 'CLASS1 IS ALPHABETIC or numeric'.
