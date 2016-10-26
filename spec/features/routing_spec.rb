describe 'Routing' do
  describe 'multi-counter' do
    it 'routes to the page that has the multi-counter example' do
      visit '/'

      click_on 'Multi-Counter'

      expect(page).to have_current_path('/multi_counter', only_path: true)
      expect(page).to have_text("Multi-Counter")
    end
  end
end
