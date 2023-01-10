# Bakalarska-prace-Hogg-Eco-anxiety-Scale
Tento repozitář obsahuje doplňující soubory k mé [bakalářské práci](https://is.muni.cz/auth/th/mhw60/), ve které jsem přeložil a ověřoval psychometrické vlastnosti Hoggovy škály environmentální úzkosti, originál viz [Hogg et al. 2021](https://doi.org/10.1016/j.gloenvcha.2021.102391).

Vzhledem k časové tísni, ve které jsem práci dokončoval, obsahuje text několik věcných chyb a nepřesností. Cílem tohoto repozitáře je tedy opravit tyto nepřesnosti, ale především umožnit, tak, jak jsem se v textu zavázal, bez problému replikovat v práci použité analýzy či znovupoužít získaná data.

V tuto chvíli repozitář obsahuje 3 datasety - první ze srpnového sběru dat (soubor STUDY1-29-08-2022.CSV), druhý z podzimního sběru dat (STUDY2-10-12-2022.CSV) a třetí "longitudinální", který obsahuje spojená data ze srpnového a podzimního sběru. Proměnné v jednotlivých datasetech, jejich popis, znění otázek a jejich škály jsou popsány v codebooku (Codebook.docx).

Pro replikování analýzy by mělo stačit stáhnout všechny soubory do jedné složky, v R studiu otevřít skript (RCODEFINAL.R) a postupně spouštět kód od začátku.

Důležitým upozorněním je, že všechny výsledky uvedené v práci byly získány za pomocí verze balíčku Lavaan 0.6-12, který počítal indikátory fitu u odhadů modelu u ordinálních dat jiným způsobem, než nejnovější verze 0.6-13 z ledna 2023. Tato verze umožňuje počítat robustní testové statistiky (RMSEA, CFI, TLI) i u ordinálních dat, které ukazují na výrazně horší fit jednotlivých modelů, než popisuji v práci. Z mých poznatků se jeví, že tato nová metoda kalkulace robustních indikátorů fitu výrazně penalizuje menší vzorky dat. Analýza matic residuálních korelací u odhadů jak čtyřfaktorového, tak bifaktor(s-1) modelu, ukazuje výbornou shodu s daty, což tyto robustní indikátory nereflektují. Tato pasáž bude doplněna po přečtení studií, na kterých jsou nové způsoby kalkulace indikátorů fitu v balíčku Lavaan založeny. 
