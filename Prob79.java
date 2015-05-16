import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

public class Prob79 {

    public static void main(String[] args) {
        String[] keylogs = readKeylogs();

        char[] keyCharacters = getKeyCharacters(keylogs);
        char[] keyCharactersInOrder = new char[keyCharacters.length];

        for (char c: keyCharacters) {
            int position = getKeyCharactersInfront(c, keylogs).length;
            keyCharactersInOrder[position] = c;
        }

        System.out.println(keyCharactersInOrder);
    }

    private static String[] readKeylogs () {
        List<String> keylogList = new ArrayList<String>();
        final String keylogFile = "/home/trhura/ProjectEuler/src/com/trhura/euler/p079_keylog.txt";

        try {
            BufferedReader reader = new BufferedReader(new FileReader(keylogFile));
            String line;

            while ((line = reader.readLine()) != null) {
                keylogList.add(line);
            }
            reader.close();
        } catch (Exception e) {
            System.err.print(e);
        }

        String[] keylogArray = new String[keylogList.size()];
        keylogList.toArray(keylogArray);

        return keylogArray;
    }

    private static char[] getKeyCharacters(String[] keylogs) {
        Set<Character> characterSet = new HashSet<Character>();

        for(String keylog: keylogs) {
            for (Character character: keylog.toCharArray()) {
                characterSet.add(character);
            }
        }


        char[] characters = new char[characterSet.size()];
        Iterator<Character> iterator = characterSet.iterator();
        for (int i = 0; i < characters.length; i++) {
            characters[i] = iterator.next().charValue();
        }

        return characters;
    }

    private static char[] getKeyCharactersInfront(Character c, String[] keylogs) {
        Set<Character> characterSet = new HashSet<Character>();

        for(String keylog: keylogs) {
            int cidx = keylog.indexOf(c);

            if (cidx != -1) {
                for (int i = 0; i < cidx; i++) {
                    characterSet.add(keylog.charAt(i));
                }
            }
        }

        char[] characters = new char[characterSet.size()];
        Iterator<Character> iterator = characterSet.iterator();
        for (int i = 0; i < characters.length; i++) {
            characters[i] = iterator.next().charValue();
        }
        return characters;
    }

}
