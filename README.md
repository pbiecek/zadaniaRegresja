# Zadania z analizy regresji - konspekt

## Zagajenie

Moja ogólna wizja fabularna jest taka, że oto pewien profesor poszukuje kogoś, kto wesprze go w przeprowadzeniu analiz statystycznych wyników badania osiągnięć uczniów szkół ponadgimnazjalnych. Jako dane wykorzystywane są nieco pozmieniane na potrzeby zadań wyniki uzyskane w części ponadgimnazjalnej (realizowanej w Polsce dodatkowo, oprócz badania piętnastolatków) badania PISA 2009 i założonego na tej próbie ucznióW panelu "Nasza Dalsza Nauka i Praca", zrealizowanego w latach 2010-2011.

## Zadanie 1. Najzwyklejsza w świecie korelacja

Gracz otrzymuje zbiór danych z prośbą o obliczenie korelacji pomiędzy wynikami testów umiejętności opisywanych przez zmienne *MATH_2009*, *READ_2009* i *SCIE_2009*, a wskaźnikiem statusu społeczno-ekonomicznego rodziny ucznia opisywanego zmienną *hisei*.

Podpowiedź: Zapewne powinieneś użyć funkcji *cor()*.

Jako odpowiedź należy podać wektor zawierający wartości trzech korelacji.

## Zadanie 2. Diagnostyka liniowego przebiegu zależności

Okazuje się, że obliczone korelacje są z grubsza o połowę z małe co do wartości bezwzględnej, a do tego mają zły znak: zależności są negatywne, a powinny być pozytywne. Chyba coś jest nie tak ze zbiorem danych! Profesor jest pewien, że zmienne od 1. do 16., a więc i te opisująca wyniki testów, są w porządku, ale przypomina sobie, że przygotowując zbiór nazwy dalszych zmiennych doklejał ręcznie w arkuszu kalkulacyjnym i podejrzewa, że mógł przy tym pomieszać ich kolejność. Jest jednak pewien, że zależność pomiędzy *READ_2009* a *hisei* ma charakter liniowy i jest to związek pozytywny. Zadaniem dla gracza jest zidentyfikowanie, która zmienna w zbiorze opisuje wskaźnik HISEI.

Podpowiedź: Jeśli zależność jest liniowa, to nie da się poprawić jakości przewidywania wprowadzając do regresji jej nieliniowe przekształcenia.

Jako odpowiedź należy podać nazwę zmiennej.

Dane zostały nieco "podkręcone", żeby zależność pomiędzy *READ_2009* a (prawdziwym) HISEI była w istocie niemalże całkiem perfekcyjnie liniowa, a pozostałe zmienne wybrane zostały do zbioru tak, aby albo nie były tak ślicznie liniowe względem *READ_2009* albo pozostawały z tą zmienną w związku negatywnym.

## Zadanie 3. Przekształcenie zmiennej niezależnej

Profesor znalazł na swoim komputerze zbiór z poprawnymi nazwami zmiennych, więc ten problem nie będzie już nas musiał dłużej zaprzątać. Prosi też gracza o nowe analizy. Zmienna *income* opisująca dochody gospodarstwa domowego ucznia okazuje się nieistotna w modelu regresji, w którym wykorzystywana jest razem ze zmienną *cultpos* (opisującą dostępność dóbr kultury w ramach gosp. dom.) do przewidywania wyników testu czytania (*READ_2009*). Niemniej wydaje się, że dochody powinny mieć istotne (choć pewnie raczej niewielkie) znaczenie, nawet przy kontroli dostępu do zasobów kulturalnych. Może problem sprawia specyficzny rozkład zmiennej *income* i jakieś (nieliniowe) przekształcenie tej zmiennej pozwoliłoby lepiej opisać wpływ dochodów na wyniki?

Podpowiedź: Rozkład *income* jest, w odróżnieniu od *READ_2009* silnie prawoskośny. Jakie przekształcenie może uczynić go bardziej symetrycznym?

Jako odpowiedź należy podać wyrażenie (expression) zawierające wzór przekształcenia. Może ono zawierać wyłącznie jedną zmienną: *income*. Sprawdzenie poprawności rozwiązania będzie się odbywać poprzez obliczenie zmiennej zgodnie z zadanym wyrażeniem, a następnie kontrolę istotności związanego z nią parametru w modelu regresji. Cokolwiek zdoła zjechać poniżej 0,05 będzie akceptowane. Dodatkowo jeśli ktoś użył logarytmu zostanie pochwalony, a jeśli nie, zostanie pouczony, że użycie logarytmu miałoby pewien walor z punktu widzenia interpretacji współczynników modelu.

Zmienna *income* została przeze mnie wtórnie wytworzona jako przekształcenie oryginalnej zmiennej *wealth* (zawierającej oceny czynnikowe oszacowane na podstawie baterii pytań o dostępność różnych rodzajów dóbr w gosp. dom.) tak, aby *income* mogła uchodzić za dochody gosp. dom. wyrażone w tysiącach złotych.

## Zadanie 4. Współliniowość

Profesor zwraca się do nas z takim problemem: oto każda ze zmiennych niezależnych użytych w modelu (mamy tu płeć, typ szkoły, grupę wskaźników statusu społeczno-ekonomicznego rodziny i grupę wyników testów psychologicznych) istotnie koreluje z wynikami testu czytania (*READ_2009*), ale kiedy włożyć do regresji MNK je wszystkie, to większość z nich nie chce być istotna. 
Profesor prosi gracza o takie zredukowanie modelu, aby wszystkie zmienne były w nim istotne statystycznie na poziomie istotności 0,05, a przy tym, żeby usunąć jak najmniej zmiennych niezależnych względem wyjściowej specyfikacji.
Podpowiedź: Profesorowi obiło się kiedyś o uszy, że w regresji zdarza się problem ze współliniwością - może ona ma coś wspólnego z tym problemem?

Podpowiedź: Oczywiście można automatycznie sprawdzać skutki usuwania poszczególnych kombinacji zmiennych, ale może warto podchwycić trop współliniwości? Pakiet car zawiera funkcję, która okaże się w takim przypadku pomocna.

Jako odpowiedź należy podać wektor nazw zmiennych do usunięcia z modelu (przy sprawdzaniu poprawności rozwiązania trzeba pamiętać o ustaleniu kolejności!).

Zmienne *csesi* i *ZAMP_WYN* zostały spreparowane na potrzeby zadania.

## Zadanie 5. Regresja w podgrupach

Teraz profesor chciałby dowiedzieć się, jakie są wartości współczynników regresji MNK wyników testu czytania (*READ_2009*) ze względu na jeden ze wskaźników statusu społeczno-ekonomicznego rodziny (*hisei*) w poszczególnych szkołach.

Podpowiedź: Można oczywiście estymować kolejno wiele modeli w podgrupach (przydałaby się wtedy automatyzacja), ale można też wprowadzić do modelu parametry interakcji.

Jako odpowiedź należy podać data frame zawierającą dwie kolumny: *SCHOOL_ID* oraz *par_hisei*. Sprawdzenie poprawności rozwiązania przez sprwdzenie struktury (że data frame, że ma odpowiednią liczbę kolumn i wierszy, że kolumny są odpowiednich typóW), posortowanie ze względu na *SCHOOL_ID* i przyrównanie all.equal() *par_hisei* do wzorca.

## Zadanie 6. Interakcja I

średnia wartość współczynnika nachylenia regresji w grupie badanych szkół wynosi 0,434. Profesora interesuje teraz, dla których szkół można uznać, że obliczony dla nich współczynnik nachylenia regresji jest istotnie statystycznie większy, a dla których, że jest istotnie statystycznie mniejszy od tej średniej. Do testowania istotności takich różnic należy użyć testów Walda (lub równoważnie testów F) przeprowadzonych w oparciu o wyestymowane parametry modelu, który zostaje podany graczowi.

Podpowiedź: W celu weryfikacji takich hipotez można użyć funkcji *linearHypothesis()* z pakietu car. Można też samodzielnie pomęczyć się z wykorzystaniem informacji zawartych w macierzy kowariancji współczynników modelu (*vcov(model)*). Jeszcze inne rozwiązanie to dokonanie reparametryzacji modelu, tak aby interesujące nas różnice stały się wprost parametrami modelu (i w efekcie ich istotności były widoczne po prostu w *summary(model)*) - może pomoże inne kodowanie kontrastów dla factora?

Jako odpowiedź należy podać wektor zawierający *ID_SCHOOL* szkół, dla których różnica jest statystycznie istotna (przy sprawdzaniu poprawności rozwiązania trzeba pamiętać o ustaleniu kolejności!). Jako poprawne akceptowane będą dwa rozwiązania: zarówno takie, które zostanie uzyskane z wzięciem pod uwagę błędu szacowania średniej, jak i takie, które tego błędu nie uwzględnia (w pierwszym przypadku dla jednej szkoły więcej różnica okazuje się istotna).

## Zadanie 7. Interakcja II

Profesor prosi o przeanalizowanie zależności wyników testu czytania (*READ_2009*) od wieku uczniów (*RAVEN_AGE*). Ostrzega przy tym, że zależność ta zapewne nie jest ciągła, bo w zbiorze są też uczniowie, którzy zaczęli ją rok wcześniej, a to jest grupa, która jest wyraźnie "pozytywnie przeselekcjonowana", oraz tacy, którym nauka się wydłużyła, a oni wprost przeciwnie są "przeselekcjonowani negatywnie". Czy gracz poradzi sobie z takim wyspecyfikowaniem modelu, w którym wiek będzie modelowany w adekwatny sposób?

Podpowiedź: Do zorientowania się, jaka specyfikacja modelu może być odpowiednia na początku najlepiej użyć diagnostyki graficznej. Można w tym celu wykorzystać np. pakiet ggplot2, w szczególności zaś wykres wykorzystujący połaczenie *geom_point()* i *geom_smooth()*.

Jako odpowiedź należy podać listę, której pierwszy element to formuła modelu. Kolejne elementy listy muszą wystąpić wtedy i tylko wtedy, gdy po prawej stronie formuły modelu występują dodatkowo zmienne inne niż *RAVEN_AGE* (do ich tworzenia można jednak wykorzystać tylko *RAVEN_AGE*). Przykładowo dwa możliwe i równoważne sobie sposoby podania hipotetycznej odpowiedzi to: *odp = list(READ_2009 ~ RAVEN_AGE + I(RAVEN_AGE^3))* oraz *odp = list(READ_2009 ~ RAVEN_AGE + RAVEN_AGE_P3, expression(RAVEN_AGE_P3 = RAVEN_AGE^3))*. Sprawdzenie poprawności będzie polegać na przyrównaniu *deviance* zaproponowanego przez gracza modelu do założonego progu.
