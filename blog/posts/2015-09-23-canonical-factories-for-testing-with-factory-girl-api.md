    Title: Canonical factories for testing with factory_girl_api
    Date: 2015-09-23T16:30:12
    Tags: ruby, rails, javascript, angular

Modern web applications are often built as *single-page apps*, which are great for keeping concerns separated, but problematic when tested. Logic needs to be duplicated in front- and back-end test suites, and if the two apps diverge, the tests won't catch the failure. I haven't found a very good solution to this problem aside from brittle, end-to-end integration tests.

To attempt to address a fraction of this problem, I built [factory_girl_api][factory_girl_api], a way to share context setup between both sides of the application.

# A brief overview of factory_girl

In the land of Ruby and Rails, [factory_girl][factory_girl] is a convenient gem for managing factories for models. Out of the box, it integrates with Rails' default ORM, ActiveRecord, and provides declarative syntax for describing what attributes factories should initialize. For example, a factory declaration used to create a widget might look like this:

```ruby
FactoryGirl.define do
  factory :widget do
    sequence(:name) { |id| 'Widget #' + id }
    price 10

    trait :expensive do
      price 1000
    end
  end
end
```

This makes it easy to create new instances of `Widget` and use them for unit tests. For example, this would create and persist a widget with a unique name and a price of 10 units:

```ruby
widget = FactoryGirl.create :widget
```

We can also create more expensive widgets by using the `:expensive` trait.

```ruby
expensive_widget = FactoryGirl.create :widget, :expensive
```

Any number of traits can be specified at once. Additionally, it is possible to override individual attributes manually.

```ruby
fancy_widget = FactoryGirl.create :widget, :expensive, name: 'Fancy Widget'
```

It works well, and it keeps initialization boilerplate out of individual tests.

# Testing on the front-end

Trouble arises when we need to write tests for the JavaScript application that use the same models. Suddenly, we need to duplicate the same kind of logic in our front-end tests. We might start out by setting up object state manually:

```js
var fancyWidget = new Widget({
  name: 'Fancy Widget',
  price: 1000
});
```

Things can quickly get out of hand when models grow complex. Even if we use a factory library in JavaScript, it's possible for our front-end factories to diverge from their back-end counterparts. This means our integration tests will fail, but our unit tests will still blindly pass. Having to duplicate all that logic in two places is dangerous. It would be nice to have a *single, canonical source* for all of our factories.

## Reusing server-side factories with factory_girl_api

To help alleviate this problem, I created the [factory_girl_api][factory_girl_api] gem for Rails and the [angular-factory-girl-api][angular-factory-girl-api] Bower package for Angular. These packages cooperate with each other to allow server-side factories to be used in JavaScript tests.

The Angular module provides a service with syntax comparable to factory_girl itself. Both traits and custom attributes are supported:

```js
FactoryGirl.create('widget', 'expensive', { name: 'Fancy Widget' });
```

In this case, however, a round-trip API call must be made to the server in order to call the factory and return the result. Because of this, the Angular version of FactoryGirl returns a promise that is resolved with the serialized version of the model, which can then be used as sample data in unit tests.

## The problems with relying on the server for data

In my preliminary use of this tool, it works. In many ways, it's much nicer than duplicating logic in both places. However, I'm not *completely* convinced it's the right solution yet.

First of all, it couples the front-end to the back-end, even during unit testing, which is disappointing. It means that a server needs to be running (in test mode) in order for the tests to run at all. For the kinds of projects I work on, this isn't really a bad thing, and the benefits of the reduced duplication far outweigh the downsides.

My real concern is that this solves a very small facet of the general problem with fragile front-end test suites. Single-page applications usually depend wholly on their integration with back-end APIs. If those APIs change, the tests will continue to happily pass as long as the API is simply mocked, which seems to be the usual solution in the front-end universe. This is, frankly, unacceptable in real application development.

## Potential improvements and other paths to success

I am ultimately unsatisfied with this approach, but writing brittle end-to-end integration tests is not the solution. This *kind* of thing may be a step in the right direction: writing tests that aren't really pure unit tests, but also aren't fragile full-stack integration tests. This is a middle-ground that seems infrequently traveled, perhaps due to a lack of tooling (or perhaps because it just doesn't work). I don't know.

Either way, I'm interested in where this is headed, and I'll be curious to see if I run into any roadblocks using the workflow I've created. If anyone else is interested in playing with these two libraries, the READMEs are much more comprehensive than what I've covered here. Take a look, and give them a spin!

- [factory_girl_api][factory_girl_api]
- [angular-factory-girl-api][angular-factory-girl-api]

[factory_girl]: https://github.com/thoughtbot/factory_girl
[factory_girl_api]: https://github.com/lexi-lambda/factory_girl_api
[angular-factory-girl-api]: https://github.com/lexi-lambda/angular-factory-girl-api
