// Marco Túlio Mello Silva - 12548657

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.io.BufferedReader;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Arrays;

public class Main {
    public static final String FILE_DELIMITER = ",";
    public static final String CSV_FILE = "dados.csv";
    public static void main(String[] args) {
        List<Paises> data;
        try(BufferedReader br = Files.newBufferedReader(Paths.get(CSV_FILE))) {
             data = br.lines()
            .map(line -> List.of(line.split(FILE_DELIMITER)))
            .map(row -> new Paises(
                row.get(0),
                Integer.parseInt(row.get(1)),
                Integer.parseInt(row.get(2)),
                Integer.parseInt(row.get(3)),
                Integer.parseInt(row.get(4))
            ))
            .collect(Collectors.toList());
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
        Scanner scanner = new Scanner(System.in);
        ArrayList<Integer> numbers = new ArrayList<>();
        String[] entry = scanner.nextLine().split(" ");
        Arrays.stream(entry)
            .mapToInt(Integer::parseInt)
            .forEach(numbers::add);
            scanner.close();
        
        System.out.println(data.stream()
        .filter(pais -> pais.confirmed > numbers.get(0))
        .map(pais -> pais.active)
        .reduce(0, Integer::sum));

        System.out.println(data.stream()
        .sorted((p1, p2) -> Integer.compare(p2.active, p1.active))
        .limit(numbers.get(1))
        .sorted((p1, p2) -> Integer.compare(p1.confirmed, p2.confirmed))
        .limit(numbers.get(2))
        .map(pais -> pais.deaths)
        .reduce(0, Integer::sum));

        System.out.println(data.stream()
        .sorted((p1, p2) -> Integer.compare(p2.confirmed, p1.confirmed))
        .limit(numbers.get(3))
        .map(pais -> pais.nome)
        .sorted()
        .collect(Collectors.joining("\n")));

    }
}

final class Paises {
    public String nome;
    public int confirmed;
    public int deaths;
    public int recovered;
    public int active;

    public Paises(String nome, int confirmed, int deaths, int recovered, int active) {
        this.nome = nome;
        this.confirmed = confirmed;
        this.deaths = deaths;
        this.recovered = recovered;
        this.active = active;
    }
}