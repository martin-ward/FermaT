package fme.components;

import java.io.File;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JMenuItem;
import javax.swing.undo.UndoableEdit;

import fme.components.console.Console;
import fme.components.tree.TreeGUI;

public class UndoManager {

	public static String TRANSFORMATION_APPLIED = "Apply transformation";

	private static UndoManager singleton = new UndoManager();

	private JMenuItem undoButton, redoButton;

	private Vector<Event> undoableEvents = new Vector<Event>();

	private Vector<Event> redoableEvents = new Vector<Event>();

	private Vector<UndoableEdit> undoableEditorEvents = new Vector<UndoableEdit>();

	private Vector<UndoableEdit> redoableEditorEvents = new Vector<UndoableEdit>();

	private UndoManager() {
	}

	public static UndoManager getInstance() {
		return singleton;
	}

	public void storeEditorEvent(UndoableEdit e) {
		undoableEditorEvents.add(e);
		undoButton.setEnabled(true);
	}

	public void storeEvent(String event, String data) {
		resetEditorEventQueue();
		undoableEvents.add(new Event(event, data));
		undoButton.setEnabled(true);
		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Store Event:" + event + " (" + data + ")");
	}

	public void undoLastEvent() {
		if (undoableEditorEvents.size() > 0) {
			redoableEditorEvents.add(undoableEditorEvents.lastElement());
			undoableEditorEvents.lastElement().undo();
			undoableEditorEvents.remove(undoableEditorEvents.size() - 1);
			redoButton.setEnabled(true);
		}

		if (undoableEvents.size() > 0) {
			redoableEvents.add(undoableEvents.lastElement());
			resetEditorEventQueue();
			undoableEvents.lastElement().undo();
			undoableEvents.remove(undoableEvents.size() - 1);
			redoButton.setEnabled(true);
		}

		// Check whether to disable buttons
		if (undoableEvents.size() == 0 && undoableEditorEvents.size() == 0) {
			undoButton.setEnabled(false);
		}
	}

	public void redoLastEvent() {
		if (redoableEditorEvents.size() > 0) {
			undoableEditorEvents.add(redoableEditorEvents.lastElement());
			redoableEditorEvents.lastElement().redo();
			redoableEditorEvents.remove(redoableEditorEvents.size() - 1);
			undoButton.setEnabled(true);
		}

		if (redoableEvents.size() > 0) {
			undoableEvents.add(redoableEvents.lastElement());
			resetEditorEventQueue();
			redoableEvents.lastElement().redo();
			redoableEvents.remove(redoableEvents.size() - 1);
			undoButton.setEnabled(true);
		}

		// Check whether to disable buttons
		if (redoableEvents.size() == 0 && redoableEditorEvents.size() == 0) {
			redoButton.setEnabled(false);
		}
	}

	public void registerUndoButton(JMenuItem undoButton) {
		this.undoButton = undoButton;
		undoButton.setEnabled(false);
	}

	public void registerRedoButton(JMenuItem redoButton) {
		this.redoButton = redoButton;
		redoButton.setEnabled(false);
	}

	public void resetEditorEventQueue() {
		// Delete all editor related events
		undoableEditorEvents.removeAllElements();
		redoableEditorEvents.removeAllElements();
	}

	private class Event {
		public String event;
		public String data;

		public Event(String event, String data) {
			this.event = event;
			this.data = data;
		}

		public void undo() {
			if (event == UndoManager.TRANSFORMATION_APPLIED) {
				Console.getInstance().loadFile(new File(data.split("#")[0]));
				((TreeGUI) ComponentRegistry.getGUIComponent("TreeGUI"))
						.expandTreeToRow(Integer.parseInt(data.split("#")[2]));
			}
		}

		public void redo() {
			if (event == UndoManager.TRANSFORMATION_APPLIED) {
				Console.getInstance().loadFile(new File(data.split("#")[1]));
			}
		}
	}
}
