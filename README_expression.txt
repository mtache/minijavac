**** DOCUMENT EXPLICATIF - PARTIE EXPRESSIONS ****

Bartosz Gorlewicz - Théophile Debauche

Pour la partie expression, nous sommes partis du TP sur les opérations avant de rajouter progressivement des fonctionnalités. 

Nous avons implémenté les "statement" basiques (for, if, while, switch, do while), les déclarations de variable en partant des types primitifs et les opérations simples.

Nous avons eu beaucoup de mal à prendre en main les outils et l'environnement, nous avons donc simplifié au maximum notre arbre (par rapport à la documentation java) pour être quand même capable de faire des choses intéressantes (comme les statement) sans se perdre dans les types de base. 

Afin de réunir les deux parties (formules et expression), comme notre arbre ne descendait pas assez bas pour vraiment inclure les expressions, nous avons divisé le type ast en deux : expression ou formules, ce qui permet de tester simplement les deux parties.
