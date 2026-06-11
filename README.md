# gh-view

Petite UI web (Elm, 100 % front, sans backend) pour suivre une liste de pull
requests GitHub choisies à la main.

## Lancer

```sh
npm install
npm run dev      # elm-live, ouvre http://localhost:8000
```

Build de production :

```sh
npm run build    # produit elm.js optimisé (à servir avec index.html + styles.css)
```

## Utilisation

1. Colle ton **token GitHub** dans le champ dédié (stocké en `localStorage`,
   bouton « Oublier » pour l'effacer). Scope nécessaire : `repo` (PAT classique)
   ou un fine-grained token avec accès lecture aux PR des dépôts visés.
2. Colle l'**URL d'une PR** (`https://github.com/owner/repo/pull/123`) et
   « Ajouter ».
3. « Rafraîchir » recharge toutes les PR **ouvertes** (les mergées/fermées sont
   figées et n'appellent plus l'API).

## Données affichées

Statut de review (approuvée / changements demandés / en attente), nombre de
commentaires **non résolus et non outdated**, mergeabilité (mergeable /
conflits / inconnu), durées relatives depuis création et dernière mise à jour
(rafraîchies chaque minute côté client), branche source (et destination si elle
n'est ni `main` ni `master`), auteur, état OPEN/MERGED/CLOSED.

Tri : PR ouvertes par date de mise à jour décroissante, puis mergées/fermées en
bas.

## Persistance

- PR **ouvertes** : seul l'identifiant est persisté, elles sont refetchées au
  chargement.
- PR **mergées/fermées** : snapshot complet gelé (affiché immédiatement, sans
  appel API).

## Limites connues

- `reviewThreads(first: 100)` : les PR avec plus de 100 fils de commentaires ne
  comptent que les 100 premiers (pas de pagination).
- Toutes les erreurs (token, 404, rate limit, réseau) sont affichées dans une
  bannière globale unique.
