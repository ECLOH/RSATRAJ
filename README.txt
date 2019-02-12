L'application est créée dans un package. 

Le dossier "/inst" contient le code source de l'application shiny. J'y ai déposé un embrillon de fichiers server.R et ui.R, à modifier et à commenter. 

Le dossier "/data" contient un objet de type seqdata, avec des données RSA simulées. A modifier si besoin en laissant le même nom, ou ajouter d'autres jeu de données ici. 

Le dossier "/R" contient tous les scripts R définissant nos fonctions. Entre autre la fonction "lancer_application()" qui permet à tous ceux qui ont installé le package de lancer l'appli. 
J'ai mal démarré mais c'est à faire pour la suite: il faut que nos fichiers ui.R et server.R soient le plus légers possibles, avec un max de code dans des fonctions. 