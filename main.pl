:- include('writer.pl').

start :-
  write('Please upload a valid profile file'), flush_output(current_output),
  load,
  
