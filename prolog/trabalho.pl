% Alvaro Braz Cunha e Diego Sanches Nere dos Santos

musica(bohemian_rhapsody, rock, queen, 70).
musica(imagine, rock, john_lennon, 70).
musica(billie_jean, pop, michael_jackson, 80).

usuario(a, rock).
usuario(b, rock).
usuario(c, pop).
usuario(d, pop).
usuario(e, rock).
usuario(f, rock).
usuario(g, pop).
usuario(h, pop).

recomendar_por_genero(Musica, Genero) :-
    musica(Musica, Genero, _, _).

recomendar_por_decada(Musica, Decada) :-
    musica(Musica, _, _, Decada).

recomendar_por_artista_e_genero(Musica, Artista, Genero) :-
    musica(Musica, Genero, Artista, _).

recomendar_por_usuario(Musica, Usuario) :-
    usuario(Usuario, Genero), recomendar_por_genero(Musica, Genero).

listar_musicas(Musica, Genero, Artista, Decada) :-
    musica(Musica, Genero, Artista, Decada).
