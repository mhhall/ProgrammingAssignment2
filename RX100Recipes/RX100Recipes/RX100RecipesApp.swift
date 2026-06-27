import SwiftUI
import SwiftData

@main
struct RX100RecipesApp: App {
    private var store = RecipeStore()

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environment(store)
                .modelContainer(for: [RecipeNote.self, RecipeFavorite.self])
        }
    }
}
