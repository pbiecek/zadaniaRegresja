# Zadania z analizy regresji - konspekt

## Zagajenie

Moja ogólna wizja fabularna jest taka, że oto pewien profesor poszukuje kogoś, kto wesprze go w przeprowadzeniu analiz statystycznych - jeśli chcesz, możesz mu pomóc.

Rozważam też przygotowaie zadania w dwóch wersjach, różniących się poziomem trudności. Różnica zasadzałaby się na tym, że w wersji prostszej wykonanie zadania byłoby zogniskowae wokół problemów statystycznych, które można rozwiązać bardzo wieloma ścieżkami, w szczególności zaś "na piechotę", bez stosowania automatyzacji. W wersji trudniejszej, problem statystyczny (czasem również nieco trudniejszy) obudowany byłby problemem z dziedziny przetwarzania danych. W ten sposóbnie trzeba by nie tylko zoperacjonalizować zadanie (określić, czego się szuka), ale również napisać automatyzację obsługującą znalezienie odpowiedzi.

## Zadanie 1. Najzwyklejsza w świecie korelacja

Gracz otrzymuje zbiór danych z prośbą o obliczenie korelacji pomiędzy XXX a YYY.

  * Wersja prostsza: nazwy zmiennych są podane po prostu, wystarczy użyć cor(), lub co tam komu innego pryjdzie do głowy.
  * Wersja trudniejsza: nasz profesor okazuje się trochę bałaganiarski i nie pamięta dokładnych nazw zmiennych, tylko ich fragmenty (jakiś wzorzecdający się łatwo przełożyć na wyrażenie regularne); trzeba je sobie znaleźć wśród wielu kolumn, jakie są w zbiorze danych, np. korzystając z grep().

## Zadanie 2. Diagnostyka liniowego przebiegu zależności

Okazuje się, że ta korelacja jest podejrzanie mała, a na dodatek ma nie ten znak, co trzeba. Chyba coś jest nie tak ze zbiorem danych! Profesor jest pewien, że zmienna opisująca XXX jest w porządku, ale przypomina sobie, że przygotowując zbiór nazwy dalszych zmiennych doklejał ręcznie w arkuszu kalkulacyjnym i podejrzewa, że mógł przy tym pomieszać ich kolejność. Jest jednak pewien, że zależność pomiędzy XXX a YYY ma charakter liniowy i jest to związek pozytywny. Zadaniem dla gracza jest zidentyfikowanie, która zmienna w zbiorze opisuje YYY.

  * Wersja prostsza: zbiór danych zawiera tylko kilka zmiennych, więc nie ma większego problemu, żeby rozwiązać zadanie dokonując graficznej analizy zależności. Dane dobrane tak, aby prawidłowy wybór był przy takim podejściu oczywisty, jednak w taki sposób aby wcale nie była to zależność z największym R^2 spośród rozpatrywanych.
  * Wersja trudniejsza: jako że zmiennych w zbiorze jest multum (duże kilkadziesiąt), analiza graficzna raczej nie wchodzi w grę. Gracz musi sparametryzować sobie problem, np. ustalając, że szuka takiej zmiennej, dla której dodanie do modelu regresji jej nieliniowego przekształcenia nie pomaga zwiększyć R^2, a następnie zakodować automatyzację pozwalającą wybrać tę zmienną, która optymalizuje przyjęte kryterium i jest w pozytywnym związku z XXX. Również w tym przypadku zmienna dająca po prostu najsilniejszą korelację z XXX nie będzie poprawną odpowiedzią. 

## Zadanie 3. Przekształcenie zmiennej niezależnej

Profesor znalazł na swoim komputerze zbiór z poprawnymi nazwami zmiennych, więc ten problem nie będzie już nas musiał dłużej zaprzątać. Prosi też gracza o nowe analizy. Zmienna WWW (zarobki lub liczba mieszkańców) jest w modelu nieistotna, a wydaje się, że powinna odgrywać zauważalną rolę. Może należałoby ją jakoś przekształcić? 

  * wersja prosta i trudniejsza tym razem bez różnicy - zdiagnozować rozkłady zmiennych i zauważyć, że rozkład WWW jest prawoskośny; wymyślić, że należy go zlogarytmizować lub spierwiastkować i to zrobić; testowanie rozwiązania musi zawierać pewien margines błędu, żeby np. logarytm nie był jedynym słusznym rozwiązaniem;
  * rozróżnienie na wersje trudności można tu uwzględnić poprzez szczegółowość ew. podpowiedzi;

## Zadanie 4. Współliniowość

Profesor zwraca się do nas z takim problemem: oto każda ze zmiennych niezależnych użytych w modelu istotnie koreluje ze zmienną zależną, ale kiedy  włożyć do regresji je wszystkie, to żadna nie chce być istotna. 
Mamy model z zestawem kilku zmiennych, które oddzielnie silnie korelują ze zmienną zależną, ale jak je użyć w modelu razem, to nie chcą być istotne. Profesor obiło się kiedyś o uszy, że w regresji zdarza się problem ze współliniwością - może ona ma coś wspólnego z tym problemem? Profesor prosi gracza o takie zredukowanie modelu, aby wszystkie zmienne były w nim istotne statystycznie, a przy tym, żeby usunąć jak najmniej zmiennych niezależnych względem wyjściowej specyfikacji.

  * Wersje trudności różnią się poziomem skomplikowania wyjściowego modelu i liczbą zmiennych, które trzeba usunąć.

## Zadanie 5. Regresja w podgrupach

Teraz profesor chciałby dowiedzieć się, jakie są wartości współczynników regresji MNK XXX ze względu na ZZZ w grupach wyróżnionych ze względu na VVV.

  * Wersja łatwiejsza: VVV opisuje podział na dwie grupy, więc nic nie stoi na przeszkodzie, aby zrobić sobie "ręcznie" dwie regresje, każdą na innym podzbiorze.
  * Wersja trudniejsza: VVV opisuje podział na kilkadziesiąt grup, więc żeby podać wynik trzeba albo zapuścić automatyzację modelowania w podgrupach, albo odpalić model z interakcją VVV z ZZZ i odpowiednio przekształcić potem jego parametry.

## Zadanie 6. Interakcja I

Profesor przypomina sobie, że chciał się jeszcze dowiedzieć, czy te różnice w wartościach parametrów między grupami są statystycznie istotne.

  * Wersja łatwiejsza: trzeba odpalić na całych danych model z interakcją i jako odpowiedź podać istotność.
  * Wersja trudniejsza: tutaj w treści prośby doprecyzowane jest, że chodzi o podanie macierzy zawierających istotności różnic, obliczone na podstawie modelu estymowanego na całych danych, z interakcją, bez żadnych poprawek na wielokrotne porównania. Z problemem można sobie poradzić albo używając funkcji przez kogoś napisanej, jak np. linearHypothesis() z pakietu 'car' (obudowując ją automatyzacją do generowania "schematu" hipotezy), albo ręcznie operując na macierzy kowariancji współczynników modelu, wykorzystując wzór na wariancję różnicy dwóch zmiennych.

## Zadanie 7. Interakcja II

Profesor prosi o przeanalizowanie zależności wyników testu od wieku uczniów (w miesiącach). Ostrzega przy tym, że zależność ta zapewne nie jest ciągła, bo w zbiorze są też uczniowie, którzy zaczęli ją rok wcześniej, a to jest grupa, która jest wyraźnie "pozytywnie przeselekcjonowana", oraz tacy, którym nauka się wydłużyła, a oni wprost przeciwnie są "przeselekcjonowani negatywnie". Czy gracz poradzi sobie z takim wyspecyfikowaniem modelu, w którym wiek będzie modelowany w adekwatny sposób? (do sprawdzenia zadania należy po prostu wykorzystywać wartość statystyki F modelu)

  * Wersja łatwiejsza: punkt przełamania zostają explicite podane.
  * Wersja trudniejsza: punkty przełamania należy samemu zidentyfikować (ach to roztargnienie profesora!), np. przy pomocy diagnostyki graficznej z użyciem regresji nieparametrycznej.
