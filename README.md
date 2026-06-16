# gh-view

Petite UI web (Elm, 100 % front, sans backend) pour suivre une liste de pull
requests GitHub choisies à la main.

## Lancer

```sh
npm install
npm run dev      # elm-live, ouvre sur http://localhost:8000
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
3. « Rafraîchir » recharge toutes les PR **ouvertes** déjà suivies (les
   mergées/fermées sont figées et n'appellent plus l'API).

### Découverte automatique

Dans **Configuration de la découverte** (bloc repliable), renseigne deux listes
globales, persistées en `localStorage` :

- des **dépôts** au format `owner/repo` ;
- des **comptes** GitHub (login, avec ou sans `@` — le `@` est retiré au
  stockage).

« **Récupérer les nouvelles PR** » lance alors deux recherches GitHub :

- les PR **ouvertes des dépôts configurés dont l'auteur est l'un des comptes
  configurés** (déclenchée seulement si les deux listes sont non vides) ;
- les PR **ouvertes qui te sont assignées**, partout (`assignee:@me`).

Les **drafts sont exclus** des deux recherches (`-is:draft`). Une PR draft peut
toujours être suivie en l'ajoutant manuellement par son URL.

Les résultats sont **fusionnés** dans la liste (dédupliqués par identifiant) :
les nouvelles PR sont ajoutées, les PR déjà connues sont mises à jour au
passage. Rien n'est jamais supprimé automatiquement ; retirer un dépôt ou un
compte n'affecte que les découvertes suivantes. Chaque recherche est plafonnée à
100 résultats : au-delà, une bannière signale que des PR n'ont pas été chargées.

## Données affichées

Statut de review (approuvée / changements demandés / en attente), nombre de
commentaires **non résolus et non outdated**, mergeabilité (mergeable /
conflits / inconnu), durées relatives depuis création et dernière mise à jour
(rafraîchies chaque minute côté client), branche source (et destination si elle
n'est ni `main` ni `master`), auteur, état OPEN/MERGED/CLOSED.

**État CI** (PR ouvertes uniquement) : deux badges agrégés dérivés du
`statusCheckRollup` GitHub du dernier commit — **GHA** (GitHub Actions) et
**CircleCI** — chacun `✓` (succeed), `✗` (failed) ou `…` (en cours), masqué si
le fournisseur n'a aucun check. Les badges sont cliquables (GHA → onglet
Actions, CircleCI → pipeline). Les autres checks (SonarCloud, Codecov…) sont
ignorés. La CI n'est pas persistée : elle est refetchée au Rafraîchir.

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
- `statusCheckRollup … contexts(first: 100)` : au-delà de 100 checks sur le
  commit de tête, les suivants ne sont pas pris en compte dans l'agrégation CI.
- Toutes les erreurs (token, 404, rate limit, réseau) sont affichées dans une
  bannière globale unique.
