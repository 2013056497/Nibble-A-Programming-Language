import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.awt.event.*;
import java.util.Scanner;
import java.io.*;

public class NibbleIDE extends JFrame implements ActionListener {
	//private TextArea textArea = new TextArea("", 0,0, TextArea.SCROLLBARS_VERTICAL_ONLY);
	private JTextArea textArea = new JTextArea("", 0,0);
	JScrollPane scrollPane = new JScrollPane(textArea);
	TextLineNumber tln = new TextLineNumber(textArea);
	private MenuBar menuBar = new MenuBar(); // first, create a MenuBar item
	private Menu file = new Menu(); // our File menu
	// what's going in File? let's see...
	private MenuItem openFile = new MenuItem();  // an open option
	private MenuItem saveFile = new MenuItem(); // a save option
	private MenuItem close = new MenuItem(); // and a close option!
	
	private Menu run = new Menu(); 
	private MenuItem lexer = new MenuItem();  
	private MenuItem parser = new MenuItem();  
	private MenuItem interp = new MenuItem();  
	
	private Panel south = new Panel();

	private JTextArea console = new JTextArea();
	//private TextAreaOutputStream taos = new TextAreaOutputStream(console , 60 );
	private CustomOutputStream cos = new CustomOutputStream(console);
    //private PrintStream ps = new PrintStream( taos );
    private PrintStream ps = new PrintStream( cos );
	private JScrollPane consoleScrollPane = new JScrollPane(console);


	private JTextField inputbox = new JTextField();

	private PrintStream stdout = System.out;
	
	
	public NibbleIDE() {
		this.setSize(640, 480); // set the initial size of the window
		this.setTitle("Nibble IDE"); // set the title of the window
		setDefaultCloseOperation(EXIT_ON_CLOSE); // set the default close operation (exit when it gets closed)
		this.textArea.setFont(new Font("Century Gothic", Font.BOLD, 12)); // set a default font for the TextArea
		this.textArea.setTabSize(1);
		// this is why we didn't have to worry about the size of the TextArea!
		this.getContentPane().setLayout(new BorderLayout()); // the BorderLayout bit makes it fill it automatically
		scrollPane.setRowHeaderView( tln );
		this.getContentPane().add(scrollPane);

		// add our menu bar into the GUI
		this.setMenuBar(this.menuBar);
		this.menuBar.add(this.file); // we'll configure this later
		
		// first off, the design of the menuBar itself. Pretty simple, all we need to do
		// is add a couple of menus, which will be populated later on
		this.file.setLabel("File");
		
		// now it's time to work with the menu. I'm only going to add a basic File menu
		// but you could add more!
		
		// now we can start working on the content of the menu~ this gets a little repetitive,
		// so please bare with me!
		
		// time for the repetitive stuff. let's add the "Open" option
		this.openFile.setLabel("Open"); // set the label of the menu item
		this.openFile.addActionListener(this); // add an action listener (so we know when it's been clicked
		this.openFile.setShortcut(new MenuShortcut(KeyEvent.VK_O, false)); // set a keyboard shortcut
		this.file.add(this.openFile); // add it to the "File" menu
		
		// and the save...
		this.saveFile.setLabel("Save");
		this.saveFile.addActionListener(this);
		this.saveFile.setShortcut(new MenuShortcut(KeyEvent.VK_S, false));
		this.file.add(this.saveFile);
		
		// and finally, the close option
		this.close.setLabel("Close");
		// along with our "CTRL+F4" shortcut to close the window, we also have
		// the default closer, as stated at the beginning of this tutorial.
		// this means that we actually have TWO shortcuts to close:
		// 1) the default close operation (example, Alt+F4 on Windows)
		// 2) CTRL+F4, which we are about to define now: (this one will appear in the label)
		this.close.setShortcut(new MenuShortcut(KeyEvent.VK_F4, false));
		this.close.addActionListener(this);
		this.file.add(this.close);
		
		this.menuBar.add(this.run);
		this.run.setLabel("Run");
		
		this.lexer.setLabel("Run lexer");
		this.lexer.addActionListener(this);
		this.lexer.setShortcut(new MenuShortcut(KeyEvent.VK_R, false));
		this.run.add(this.lexer);

		this.parser.setLabel("Run parser");
		this.parser.addActionListener(this);
		this.parser.setShortcut(new MenuShortcut(KeyEvent.VK_E, false));
		this.run.add(this.parser);

		this.interp.setLabel("Run interpreter");
		this.interp.addActionListener(this);
		this.interp.setShortcut(new MenuShortcut(KeyEvent.VK_W, false));
		this.run.add(this.interp);
		
		this.console.setEditable(false);
		this.console.setSize(640,240);
		new SmartScroller( consoleScrollPane );
		System.setOut( ps );
        System.setErr( ps );
		this.consoleScrollPane.setPreferredSize(new Dimension(640, 200));

		this.inputbox.addActionListener(this);

		south.setLayout(new BorderLayout());
    	south.add(inputbox, BorderLayout.SOUTH);

		this.getContentPane().add(consoleScrollPane, BorderLayout.SOUTH );
	}
	
	public void actionPerformed (ActionEvent e) {
		// if the source of the event was our "close" option
		if (e.getSource() == this.close)
			this.dispose(); // dispose all resources and close the application
		
		// if the source was the "open" option
		else if (e.getSource() == this.openFile) {
			JFileChooser open = new JFileChooser(); // open up a file chooser (a dialog for the user to browse files to open)
			FileNameExtensionFilter filter = new FileNameExtensionFilter("Nibble source", "nbl");
			open.setFileFilter(filter);
			int option = open.showOpenDialog(this); // get the option that the user selected (approve or cancel)
			// NOTE: because we are OPENing a file, we call showOpenDialog~
			// if the user clicked OK, we have "APPROVE_OPTION"
			// so we want to open the file
			if (option == JFileChooser.APPROVE_OPTION) {
				this.textArea.setText(""); // clear the TextArea before applying the file contents
				try {
					// create a scanner to read the file (getSelectedFile().getPath() will get the path to the file)
					Scanner scan = new Scanner(new FileReader(open.getSelectedFile().getPath()));
					while (scan.hasNext()) // while there's still something to read
						this.textArea.append(scan.nextLine() + "\n"); // append the line to the TextArea
				} catch (Exception ex) { // catch any exceptions, and...
					// ...write to the debug console
					System.out.println(ex.getMessage());
				}
			}
		}
		
		// and lastly, if the source of the event was the "save" option
		else if (e.getSource() == this.saveFile) {
			JFileChooser save = new JFileChooser(); // again, open a file chooser
			FileNameExtensionFilter filter = new FileNameExtensionFilter("Nibble source", "nbl");
			save.setFileFilter(filter);
			int option = save.showSaveDialog(this); // similar to the open file, only this time we call
			// showSaveDialog instead of showOpenDialog
			// if the user clicked OK (and not cancel)
			if (option == JFileChooser.APPROVE_OPTION) {
				try {
					// create a buffered writer to write to a file
					BufferedWriter out = new BufferedWriter(new FileWriter(save.getSelectedFile().getPath()+".nbl"));
					out.write(this.textArea.getText()); // write the contents of the TextArea to the file
					out.close(); // close the file stream
				} catch (Exception ex) { // again, catch any exceptions and...
					// ...write to the debug console
					System.out.println(ex.getMessage());
				}
			}
		}
		
		else if (e.getSource() == this.lexer) {
			//taos.clear();
			cos.clear();
			System.out.println("Lexical Analysis\n");
			System.out.println(" LineNo. || <Lexeme> ==> <TokenKind>");
			NibbleScanner scanner = new NibbleScanner(this.textArea.getText());
	        NibbleToken token;
	            do {
	                scanner.next();
	                token = scanner.token();
	                System.out.printf("   %d            ||    %s ==> %s\n",
	                	token.line(), token.image(), token.tokenRep());
	            } while (token.kind() != TokenKind.EOF);
		}
		
		else if (e.getSource() == this.parser) {
			//taos.clear();
			cos.clear();
			System.out.println("Syntactic Analysis\n");
	        NibbleScanner scanner = new NibbleScanner(this.textArea.getText());
	        NibbleParser parser = new NibbleParser(scanner);
	        parser.nibbleStart();
		}

		else if (e.getSource() == this.interp) {
			//taos.clear();
			cos.clear();
			System.out.println("Interpreter in cmd...");
			System.setOut( stdout );
			//cos.clear();
			System.out.println("\n\n\nRunning nibble program...\n-----\n\n\n");
	        NibbleScanner scanner = new NibbleScanner(this.textArea.getText());
	        NibbleInterpreter interp = new NibbleInterpreter(scanner);
	        interp.nibbleStart();
	        //String sentence = sc.nextLine();
			//System.out.println(sentence);
			System.setOut( ps );
		}

		else if (e.getSource() == this.inputbox) {
			//taos.clear();
			cos.clear();
			System.out.println("PRESS ENTER\n");
		}
		
		
	}
	
	public void compile() {
		
		/*
		*/
        
		
	}
	
    // the main method, for actually creating our notepad and setting it to visible.
    public static void main(String args[]) throws InterruptedException {
        NibbleIDE app = new NibbleIDE();
        app.setVisible(true);
		
		
	}
}