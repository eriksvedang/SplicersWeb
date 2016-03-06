# Rules 0.2a

---

### Game Objective
_add presentation text here_

#### End of game
The game ends when a player dominates at least 3 biom cards at the end of a round.
Alternitavely, if a player cannot draw a card at the start of a round, that round is the last one. At the end of the round, the player with the most dominated biom cards win the game. In case of a tie, the player with biggest sum of printed dominance value on dominated biom cards win the game.

### Set-up
Each player does the following:
1. Prepare your deck by shuffling your cards (except your splicer and biom cards) and place them face down in front of you. Make room for a "discard pile" to place discarded cards.
2. Place the splicer card face up next to the deck and the biom cards face up in a row between the players.
3. Draw a number of cards (indicated on your splicer card) to form a hand of cards.
4. Randomly determine first player.

Now, in turn order starting with the first player, each player may perform a mulligan. The game then starts with the first round, skipping the start of round phase (see turn structure below).

### Card Types
There are 5 card types in Splicers:
- **Ting**
- **Mutation**
- **Event**
- **Biom**
- **Splicer**

Every deck consists of at least 18 cards, including 1 Splicer card and 2 Biom cards. There can only be one copy of each card in the deck.

#### Biom
Biom cards represent geological spaces that the players control through **domination**. Bioms have 4 areas:
- name
- type(s)
- dominance value
- ability text

If the collected dominance of one player is more than the dominance value of the biom card, and more than their opponent, they are **dominating** the card. To win the game, a player needs to dominate at least three biom cards at the end of a round.
When a player is dominating a biom card, rotate it towards the player.

#### Ting
Tings represent different entities that inhabit the different Bioms. Tings have 5 specific areas:
- name
- type(s)
- dominance value
- genes
- ability text

When playing ting cards, they are first placed face down. While a ting is face down, it is called a **seed**. A seed can later be turned face up, this is called “evolving”. While a ting is face up, it's dominance value is counted your total dominance value in that biom. Another way to bring a ting into play is to “splice” two tings, combining their genes to instantly play and evolve a ting from your deck.

#### Mutation
Mutations are cards that can be hosted on tings to alter them in different ways. Mutations have 4 areas:
- name
- type(s)
- genes
- ability text.

To play a mutation card, put it under a ting card (friendly or enemy) so that it's name is still shown (players may look at it). The ting is the mutation's **host**.
Playing a mutation is not an action (you may play a mutation when evolving a ting for it to meet a biom requirement, for example). You may however only play mutations during your turn of the action phase. If a ting is devolved while hosting a mutation, it is discarded. Mutations exhaust and unexhaust like their host card. For example, both host and mutation card are exhausted when using an exhaust ability on the mutation card.
When splicing, you may not use the genes on the mutation card.

#### Event
Event cards are cause one-shot abilites that happen at the end of a round. Events have 4 areas:
- name
- type(s)
- genes
- ability text

A player may play an event as their action. If they do, they are also passing and their turn ends. Only one player may play an event card each round. Event cards are played face down. They are revealed (and their abilities trigger) at the end of a round before win conditions are checked. They are then discarded.

#### Splicer
The Splicer card represent the character you play. Splicers have 4 areas:
- name
- type(s)
- starting hand size
- ability text

The splicer card is always in play and is not a ting or a seed. It cannot be discarded and cannot host mutation cards. At the start of the game, the starting hand size number is the number of cards you draw as your opening hand.

### Turn Structure
The game is played in rounds consisting of turns. A round ends when a player has passed.
1. **Start of round.** The player’s cards are unexhausted. Each player draw a card from their deck.
2. **Action phase.** The players take turns doing actions, starting with the first player. The player must do at least one of the following actions:
  - Evolve a ting.
  - Play a seed.
  - Use an active ability.
  - Splice.
  - Play an event and pass.
  - Pass.
3.	**End of round.** The first time each round a player passes the opponent takes one turn and then the round ends. Check win conditions. Any "end of round" effects trigger.

The player who passed first is the new first player.

### Misc. Rules

### Actions
At the beginning of each turn during the action phase, the

#### Seeds
Any card type can be played as a **seed**. When you do, place it face down and exhausted in front of a biom. A seed counts as 1 dominance towards that biom.

#### Evolving
While any card can be a seed, ting cards can be turned face up as an "evolve" action. You may only evolve unexhausted seeds. When you evolve a ting card, it is no longer a seed.

#### Abilities
The ability text may contain both passive abilites and active abilites. Passive abilites trigger depending on the game state. Active abilites are indicated by a colon (example: "@: hunt"). The icon or text before the colon is the **ability cost** which must be fulfilled for the ability to trigger, while the text after the colon is the actual ability. Active abilites can only be triggered on your turn during the action phase. Aside from the ability cost, triggering an active ability consumes one action. You may only use active abilites on your own cards. Abilites can only trigger while the card is in play face up.

#### Exhausted / Unexhausted cards
Typically, the ability cost of cards are "@" which means that the card must be exhausted for the ability to trigger. When a card is exhausted, the card is turned sideways. An exhausted card cannot be exhausted again. In the beginning of each round, any exhausted cards are unexhausted, meaning that they are no longer exhausted and turned to it's original rotation.

#### Genes & Splicing
Splicing is a way to bring cards into play without having them in your hand! Tings, mutations and events have gene-symbols that are used when splicing.

To splice, choose two of your unexhausted tings in play and exhaust them. You may now search your deck for any card that share at least one gene symbol from each ting you exhausted. If you find such a card, you may play it immediately. If it is a ting, it is played already evolved but exhausted. If it is an event, you must show it to the opponent before playing it. Note: Normal event rules apply, meaning that when playing an event card you also pass and your turn immediately ends.
When the splice action is done, shuffle your deck.

#### Requirements
Some tings and bioms have a requirement keyword (**req. [gene] / [type]**). If they do, the indicated requirement must be fulfilled for a ting to be in its biom. If the requirement is not fulfilled, the ting must be devolved. When a ting uses the roam ability, it may not move to a biom where a requirment isn't met. Tings may only require types (bioms have no genes) and bioms may only require genes.

#### Mulligan
When performing a mulligan, choose any number of cards and discard them, then draw that many cards from your deck.

#### Public Information
All face up cards (including discard pile) are public information and may be looked at at any time. The number of cards left in a deck, as well as the number of cards in hand is also public inormation. Face down cards in play may only be looked at by their owner.

---

### Keywords

#### Common
- **+X/-X** - change the dominance value by X until the end of the round.
- **close** - a ting or seed in the same biom.
- **distant** - a ting or seed not in the same biom.
- **enemy** - a ting or seed that your opponent owns.
- **friendly** - a ting or seed that you own.
- **devolve** - turn a ting face down, exhausted (it is now a seed).
- **req. [gene] / [type]** - see Misc Rules/Requirements.
- **dominating** - when a player has at least the amount dominance as a biom's dominance value and more dominance than their opponent.
- **probe** - look at face down or hidden card.
- **revive** - put a card from the discard pile to your hand.
- **backup** - when this card is discarded while in play, instead put it in your hand.
- **bounce** - put a card back in the owner's hand.
- **moulder** - shuffle into deck.

#### Ting-related
- **hunt** - you may devolve a close animal with the same or lower dominance as this ting.
- **evade** - you cannot use hunt on this animal.
- **extinguish** discard a ting.
- **expensive** - consume one more action to evolved
- **graze** - exhaust a close plant to gain two actions.
- **harvest** - discard all tokens on this card and gain as many actions as tokens discarded.
- **roam** - move card to another biom, if able.

#### Event-related
- **crunch** - discard a seed.
- **season** this event is active and not discarded until another event is played.

---
