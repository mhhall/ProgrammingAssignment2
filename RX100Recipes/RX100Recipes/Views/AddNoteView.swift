import SwiftUI
import SwiftData

struct AddNoteView: View {
    let recipeId: UUID
    let recipeName: String

    @Environment(\.dismiss) private var dismiss
    @Environment(\.modelContext) private var context

    @State private var noteText: String = ""
    @FocusState private var isFocused: Bool

    var body: some View {
        NavigationStack {
            VStack(alignment: .leading, spacing: 0) {
                Text("Notes for \(recipeName)")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .padding(.horizontal, 20)
                    .padding(.top, 12)
                    .padding(.bottom, 8)

                TextEditor(text: $noteText)
                    .focused($isFocused)
                    .font(.body)
                    .padding(.horizontal, 16)
                    .scrollContentBackground(.hidden)
                    .background(Color(.systemBackground))

                Spacer()
            }
            .background(Color(.systemBackground))
            .navigationTitle("New Note")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Cancel") { dismiss() }
                }
                ToolbarItem(placement: .confirmationAction) {
                    Button("Save") { saveNote() }
                        .disabled(noteText.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
                        .fontWeight(.semibold)
                }
            }
            .onAppear { isFocused = true }
        }
    }

    private func saveNote() {
        let trimmed = noteText.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return }
        context.insert(RecipeNote(recipeId: recipeId, text: trimmed))
        dismiss()
    }
}
