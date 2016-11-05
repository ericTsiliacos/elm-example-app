describe 'Counter' do
  after do
    page.execute_script('window.localStorage.clear()')
  end

  it 'defaults the counter to 0' do
    visit '/index.html'

    expect(page).to have_text('Count')
    expect(page).to have_text('0')
  end

  it 'increments a counter' do
    visit '/index.html'

    click_button "+"

    expect(page).to have_text('1')
  end

  it 'decrements a counter' do
    visit '/index.html'

    click_button "+"
    click_button "+"
    click_button "+"

    click_button "-"

    expect(page).to have_text('2')
  end
end
