##
## EPITECH PROJECT, 2019
## FUN_day2_2019
## File description:
## Makefile
##

SRC	=	DoOp.hs

OBJ	=	$(SRC:.hs=.o) \
		$(SRC:.hs=.hi)

NAME	=	doop

all:	$(NAME)

$(NAME):
	ghc -o $(NAME) $(SRC)
	make clean

clean:
	rm -rf $(OBJ)

fclean:	clean
	rm -f $(NAME)

re:	fclean all
