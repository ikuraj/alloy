DOUBLE CHECK THAT THIS WORKS:

/*
a nasty consequence of making univ a sig:
projection now regards anything below univ
as a subtype! the code that determines
what types get projected must be changed
to recognize the top-level sigs differently.
*/

