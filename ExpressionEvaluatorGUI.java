import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

public class ExpressionEvaluatorGUI extends JFrame {
    private JTextField inputField;
    private JTextArea outputArea;
    private JButton evaluateButton;

    public ExpressionEvaluatorGUI() {
        setTitle("C++ Expression Evaluator");
        setSize(600, 500);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setLayout(new BorderLayout());

        inputField = new JTextField();
        evaluateButton = new JButton("Evaluate");
        outputArea = new JTextArea();
        outputArea.setEditable(false);
        JScrollPane scrollPane = new JScrollPane(outputArea);

        JPanel topPanel = new JPanel(new BorderLayout());
        topPanel.add(new JLabel("Enter Expression: "), BorderLayout.WEST);
        topPanel.add(inputField, BorderLayout.CENTER);
        topPanel.add(evaluateButton, BorderLayout.EAST);

        add(topPanel, BorderLayout.NORTH);
        add(scrollPane, BorderLayout.CENTER);

        evaluateButton.addActionListener(e -> runEvaluator());
    }

    private void runEvaluator() {
        String input = inputField.getText();

        try {
            ProcessBuilder pb = new ProcessBuilder("D:\\PBL(os)\\pblComp\\complier.exe"); // use "evaluator.exe" on Windows
            Process process = pb.start();

            // Send input to the C++ program
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
            writer.write(input);
            writer.newLine();
            writer.flush();
            writer.close();

            // Read output from C++ program
            BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line;
            outputArea.setText("");
            while ((line = reader.readLine()) != null) {
                outputArea.append(line + "\n");
            }

            reader.close();
        } catch (IOException ex) {
            outputArea.setText("Error running evaluator: " + ex.getMessage());
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> new ExpressionEvaluatorGUI().setVisible(true));
    }
}
