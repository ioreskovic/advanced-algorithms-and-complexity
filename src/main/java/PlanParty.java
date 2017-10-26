import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StreamTokenizer;
import java.util.*;

class PlanParty {
    static class Vertex {
        Vertex() {
            this.weight = 0;
            this.children = new ArrayList<Integer>();
        }

        int weight;
        ArrayList<Integer> children;
    }

    static Vertex[] ReadTree() throws IOException {
        InputStreamReader input_stream = new InputStreamReader(System.in);
        BufferedReader reader = new BufferedReader(input_stream);
        StreamTokenizer tokenizer = new StreamTokenizer(reader);

        tokenizer.nextToken();
        int vertices_count = (int) tokenizer.nval;

        Vertex[] tree = new Vertex[vertices_count];

        for (int i = 0; i < vertices_count; ++i) {
            tree[i] = new Vertex();
            tokenizer.nextToken();
            tree[i].weight = (int) tokenizer.nval;
        }

        for (int i = 1; i < vertices_count; ++i) {
            tokenizer.nextToken();
            int from = (int) tokenizer.nval;
            tokenizer.nextToken();
            int to = (int) tokenizer.nval;
            tree[from - 1].children.add(to - 1);
            tree[to - 1].children.add(from - 1);
        }

        return tree;
    }

    static void dfs(Vertex[] tree, int vertex, int parent, Map<Integer, Integer> dp, Set<Integer> visited) {
        if (visited.contains(vertex)) return;
        visited.add(vertex);

        int withoutMeAndWithChildren = 0;
        int withMeAndGrandChildren = tree[vertex].weight;

        for (int child : tree[vertex].children) {
            if (child != parent) {
                dfs(tree, child, vertex, dp, visited);
            }
        }

        for (int child : tree[vertex].children) {
            if (child != parent) {
                withoutMeAndWithChildren += dp.get(child);

                for (int grandchild : tree[child].children) {
                    if (grandchild != vertex) {
                        withMeAndGrandChildren += dp.get(grandchild);
                    }
                }
            }
        }

        dp.put(vertex, Math.max(withoutMeAndWithChildren, withMeAndGrandChildren));
    }

    static int MaxWeightIndependentTreeSubset(Vertex[] tree) {
        int size = tree.length;
        if (size == 0) {
            return 0;
        }

        Map<Integer, Integer> dp = new HashMap<>();
        Set<Integer> visited = new HashSet<>();

        dfs(tree, 0, -1, dp, visited);

        return dp.values().stream().max(Integer::compareTo).get();
    }

    public static void main(String[] args) throws IOException {
        // This is to avoid stack overflow issues
        new Thread(null, new Runnable() {
            public void run() {
                try {
                    new PlanParty().run();
                } catch (IOException e) {
                }
            }
        }, "1", 1 << 26).start();
    }

    public void run() throws IOException {
        Vertex[] tree = ReadTree();
        int weight = MaxWeightIndependentTreeSubset(tree);
        System.out.println(weight);
    }
}
