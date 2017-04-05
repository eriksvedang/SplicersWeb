# Misc todos
- [x] "add card"-kort i /cards
- [x] fixa meny-länkar
- [x] fixa meny-funktion
- [x] inte kunna submitta kort utan titel
- [x] stöd för alla gener
- [x] Text på förstasidan
- [x] länkar i user page går till start, inte rätt kort
- [x] Filter cards-rutan ska vara dold som default
- [x] Leksidan: Start editing-knapp
- [x] Spelarsidan: Ta bort edit-knappen
- [x] inte kunna lägga till kort av samma namn om man inte är den som skapat kortet
- [x] Konfirmering (i javascript) för att ta bort lek (dialogruta?)
- [x] Om man tar bort en lek så tas man till spelarsidan, men man måste refresh:a för att leken ska försvinna
- [x] Delete-knappen på leksidan har ... i titeln men det är ingen konfirmering.
- [x] editing: <lektitel> gör lekens titel till lowercase, vilket kan vara irriterande om man kallat sin lek något som ser bra ut med uppercase
- [x] Vit kant ovanför färgsegmentet (med titeln) på leksidan
- [x] Bioms har 0 i domination.
- [x] Vill kunna skriva flera rader text (med radbrytning) på regeltexten.
- [x] När man skapat ett kort vill man kunna skapa ett nytt direkt från nästa sida.
- [x] Vill kunna göra en ny version av ett existerande kort lätt.
- [x] Inte kunna skapa en ny version av någon annans kort?
- [x] Korten hamnar mitt emellan sidor när man skriver ut en hel lek.
- [x] Mer whitespace i höjdled på "create card"-sidan (svårt att se vilka rubriker som hör till inputfälten)
- [x] Spelarsidan: Snyggare lista med deck och card
- [x] kryptera lösenord!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
- [x] Visa inte alla versioner av ett kort när man går in på /card
- [x] Länk tillbaka till login-sidan när man loggats ut.
- [x] filtrera kort efter ability, gener, typ
- [x] Filtrera på "mina kort".
- [x] En ikon (kvadratisk) till olika situationer när det behövs (twitter, discord, github, etc.)
- [x] Gör delete-knappen för lekar på spelarsidan till ett litet kryss.
- [x] Räknare som visar antal kort i leken.
- [x] Kortmängd syns inte på Splicer. —detta är för att exempelkortet saknar carddraw-ruta. likaså har kort som editas enbart de rutor som krävs för den korttypen!
- [x] Länk till Discord-kanalen
- [x] Objectives i rules
- [x] Gör delete-knappen för lekar på spelarsidan till ett litet kryss.
- [x] Räknare som visar antal kort i leken.
- [x] Kortsidan: Text med "Select cards for your deck"
- [x] Kortmängd syns inte på Splicer. —detta är för att exempelkortet saknar carddraw-ruta. likaså har kort som editas enbart de rutor som krävs för den korttypen!
- [x] Det står "Designed by Unknown" på Create Card-sidan, borde gå att veta vad man heter redan där

# Wishlist
- [ ] comments (på kort?)
- [ ] pagination på /cards

# Security
- [ ] kan submitta kort även om man inte är authensierad, /submit-card checkar inte auth
- [ ] kan gå in på deck-sidor utan att vara inloggad, får upp knappar för att ändra..?!

# Rules document
- [ ] Ta bort menyn från regelsidan när man printar (mindre font också, så att det tar upp färre sidor att skriva ut)

# UI / UX
- [ ] Expand funktion för keywords
- [ ] Under "add new card"-formuläret: "design guidelines" och svar på vanliga frågor när man designar kort
- [ ] Borde beskriva alla vanliga subtyper som finns (just nu är det bara två exempel på "create card"-sidan). Med länk till reglerna kring subtypes.
- [ ] Förslag på subtyper borde ändras när man skapar Biom (och länka till en lista med vanliga subtyper för biom?)
- [ ] Ska subtyp skrivas ut på Splicers / Events ? Iaf inte om de är tomma.
- [ ] Ibland blir det bara 6 kort på en sida när man printar
- [ ] Om man går in på en annan lek medans man valt 'edit' på en tidigare lek så tror sidan att man ska kunna ta bort kort ur den andra leken.
- [ ] Verkar som att den senaste versionen av kort inte alltid är den som visas på /cards ??!
- [ ] Behöver en proffsigare bild på framsidan

# PR
- [ ] Klistermärken?!
- [ ] Releasefest
- [ ] Blogpost

# Firefox
- [ ] Textfältet för regler på kortet overflowar åt höger istället för att radbryta (Firefox).
- [ ] Vänstermarginal på Player-sidan är för liten (Firefox)
- [ ] Kort "hoppar runt" när man drar musen över dem (Firefox)
- [ ] Kort är alldeles för stora på /deck när man tryckt "Edit"!!!

# Refactorings
- A type for DeckID:s
- A type for Hashed password / salt
- 
